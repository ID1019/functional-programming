-module(image).

-compile(export_all).


% --- Grayer  RGB is converter to (R+G+B) div 3) ---

grayer(Out) ->
        spawn_link(fun() -> grayer_init(Out) end).


grayer_init(Out) ->
    receive 
	{header, {P, Size, Depth}} ->
	    Out ! {header, {gray, Size, Depth}},
	    grayer_lines(P, Out)
    end.

gray({P, Size, Depth, Image}) ->
    {gray, Size, Depth, grayer_lines_seq(P, Image)}.

grayer_lines_seq(_, []) ->
    [];
grayer_lines_seq(P, [Line|Lines]) ->
    [grayer_line(P, Line) | grayer_lines_seq(P, Lines)].


grayer_lines(P, Out) ->
    receive
	{line, N, Line} ->
	    Grayer = grayer_line(P, Line),
	    Out ! {line, N, Grayer},
	    grayer_lines(P,Out);
	done ->
	    Out ! done
    end.


grayer_line(bw, Line) ->
    lists:map(fun(B) -> B*255 end, Line);
grayer_line(gray, Line) ->
    Line;
grayer_line(rgb, Line) ->
    grb_to_gray(Line).

grb_to_gray([]) ->
    [];
grb_to_gray([{R,G,B}|Line]) ->
    Gray = (R+G+B) div 3,
    [Gray|grb_to_gray(Line)].


% -- Reducer --- reduces to a gray scale of D

reducer(D, Out) ->
        spawn_link(fun() -> reducer_init(D, Out) end).

reducer_init(D, Out) ->
    receive 
	{header, {P, Size, Depth}} ->
	    Out ! {header, {gray, Size, D}},
	    F = fun(R) -> trunc((R/Depth)*D) end,
	    reduce_lines(P, F, Out)
    end.

reduce(D, {P, Size, Depth, Image}) ->
    F = fun(R) -> trunc((R/Depth)*D) end,
    {gray, Size, D, reduce_lines_seq(P, F, Image)}.

reduce_lines_seq(_, _, []) ->
    [];
reduce_lines_seq(P, D, [Line|Lines]) ->
    [reduce_line(P, D, Line) | reduce_lines_seq(P, D, Lines)].


reduce_lines(P, D, Out) ->
    receive
	{line, N, Line} ->
	    Reducer = reduce_line(P, D, Line),
	    Out ! {line, N, Reducer},
	    reduce_lines(P, D, Out);
	done ->
	    Out ! done
    end.


reduce_line(bw, D, Line) ->
    lists:map(fun(B) -> D(B) end, Line);
reduce_line(gray, D, Line) ->
    gray_to_reduced(Line, D);
reduce_line(rgb, D, Line) ->
    grb_to_reduced(Line, D).

grb_to_reduced([], _) ->
    [];
grb_to_reduced([{R,G,B}|Line], D) ->
    Reduced = {D(R), D(G), D(B)},
    [Reduced|grb_to_reduced(Line, D)].

gray_to_reduced([],_) ->
    [];
gray_to_reduced([N|Line], D) ->
    [D(N)|gray_to_reduced(Line, D)].



% -- Blurer --- takes the median of 9 pixles (only gray scale) 

blurer(Out) ->
    spawn_link(fun() -> blurer_init(Out) end).

blurer_init(Out) ->
    receive 
	{header, {P, Size, Depth}} ->
	    Out ! {header,  {P, Size, Depth}},
	    blurer_lines(P, 1, [], Out)
    end.


blur({P, Size, Depth, Image}) ->
    {gray, Size, Depth, blurer_lines_seq(P, Image)}.

blurer_lines_seq(_, []) ->
    [];
blurer_lines_seq(P, [L1|Lines]) ->
    [L1 | blurer_lines_seq(P, L1, Lines)].

blurer_lines_seq(_, _, []) ->
    [];
blurer_lines_seq(_, _, [L2]) ->
    [L2];
blurer_lines_seq(P, L1, [L2,L3|Lines]) ->
    [blurer_line(L1,L2,L3) | blurer_lines_seq(P, L2, [L3|Lines])].


blurer_lines(P, N, [], Out) -> 
    receive
	{line, N, L1} ->
	    Out ! {line, N, L1}, %% first line unmodified
	    blurer_lines(P, N+1, [L1], Out);	    
	done ->
	    Out ! done
    end;
blurer_lines(P, N, [L1,L2], Out) -> 
    receive
	{line, N, L3} ->
	    Line = blurer_line(L1,L2,L3),
	    Out ! {line, N-1, Line},
	    blurer_lines(P, N+1, [L2, L3], Out);
	done ->
	    %% last row unmodified
	    Out ! {line, N-1, L2},
	    Out ! done
    end;
blurer_lines(P, N, [L1], Out) -> 
    receive
	{line, N, L2} ->
	    blurer_lines(P, N+1, [L1, L2], Out);	    
	done ->
	    %% last row unmodified
	    Out ! {line, N-1, L1},
	    Out ! done
    end.

blurer_line([], [], []) ->    
    [];
blurer_line(L1, L2, L3) ->
    %% first pixle unmodified
    [hd(L2)|gray_to_blur(L1,L2,L3)].

gray_to_blur(_, [_,B2], _) ->
    %% last line unmodified
    [B2];
gray_to_blur([A1,A2,A3|As], [B1,B2,B3|Bs], [C1,C2,C3|Cs]) ->
    D = lists:nth(5,lists:sort([A1,A2,A3,B1,B2,B3,C1,C2,C3])),
    [D| gray_to_blur([A2,A3|As], [B2,B3|Bs], [C2,C3|Cs])].


% -- Solarer --- takes the median of 9 pixles (only gray scale) 

solarer(Out) ->
    spawn_link(fun() -> solarer_init(Out) end).

solarer_init(Out) ->
    receive 
	{header, {P, Size, Depth}} ->
	    Out ! {header,  {P, Size, Depth}},
	    solarer_lines(P, 1, [], Out)
    end.

solar({P, Size, Depth, Image}) ->
    {gray, Size, Depth, solarer_lines_seq(P, Image)}.

solarer_lines_seq(_, []) ->
    [];
solarer_lines_seq(P, [L1|Lines]) ->
    %% first line unmodifies
    [L1 | solarer_lines_seq(P, L1, Lines)].

solarer_lines_seq(_, _, []) ->
    [];
solarer_lines_seq(_, _, [L2]) ->
    %% last line unmodified
    [L2];
solarer_lines_seq(P, L1, [L2,L3|Lines]) ->
    [solarer_line(L1,L2,L3) | solarer_lines_seq(P, L2, [L3|Lines])].


solarer_lines(P, N, [], Out) -> 
    receive
	{line, N, Line} ->
	    Out ! {line, N, Line}, %% first line unmodified
	    solarer_lines(P, N+1, [Line], Out);	    
	done ->
	    Out ! done
    end;
solarer_lines(P, N, [L1,L2], Out) -> 
    receive
	{line, N, L3} ->
	    Line = solarer_line(L1,L2,L3),
	    Out ! {line, N-1, Line},
	    solarer_lines(P, N+1, [L2, L3], Out);
	done ->
	    %% last row unmodified
	    Out ! {line, N-1, L2},
	    Out ! done
    end;
solarer_lines(P, N, [L1], Out) -> 
    receive
	{line, N, L2} ->
	    solarer_lines(P, N+1, [L1, L2], Out);	    
	done ->
	    %% last row unmodified
	    Out ! {line, N-1, L1},
	    Out ! done
    end.

solarer_line([], [], []) ->    
    [];
solarer_line(L1, L2, L3) ->
    %% first pixle unmodified
    [hd(L2)|gray_to_solar(L1,L2,L3)].

gray_to_solar(_, [_,B2], _) ->
    %% last line unmodified
    [B2];
gray_to_solar([A1,A2,A3|As], [_B1,B2,B3|Bs], [C1,C2,C3|Cs]) ->
    D = solar(A1,A2,A3,C1,C2,C3),
%     D = contrast([A1, A2, A3, _B1, B2, B3, C1, C2, C3]),
    [D| gray_to_solar([A2,A3|As], [B2,B3|Bs], [C2,C3|Cs])].

%% take the average of upper and lower row
solar(A1,A2,A3,C1,C2,C3) ->
    ((-A1 -2*A2 -A3 +C1+2*C2+C3) + 1020) div 8.

%% difference between high and low      
contrast(Square) ->
    (255 - (lists:max(Square) - lists:min(Square))).
    
    


    

		       
    
