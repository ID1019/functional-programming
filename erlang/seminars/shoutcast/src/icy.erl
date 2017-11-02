%% This module will include all the details of the ICY protocol. 
%%
%% A reading process will use the parser to parse a binary encoded ICY
%% message. The parser will not read from a socket since this could
%% block a process.  Instead it will return a function (a
%% continuation) that a process can use to parse binary data from a
%% socket (or something else).

%% The strategy to extract the socket communication makes the module
%% more generic. We might want to handle other streams than sockets.

%% Specifications are from RFC 2616
%%
%%
%%        Message      = Start-Line                
%%                        *(( general-header        
%%                         | response-header        
%%                         | entity-header ) CRLF)  
%%                        CRLF
%%                        [ message-body ]          
%% 
%%        Start-Line   = Request-Line | Status-Line
%%
%%        Status-Line  = HTTP-Version SP Status-Code SP Reason-Phrase CRLF
%%
%%        Request-Line = Method SP Request-URI SP HTTP-Version CRLF

-module(icy).

-export([encode_response/1, encode_meta/1]).

-export([decode_request/1, decode_version/1, decode_headers/2]).

-export([segments/1]).


-define(Chunk, 24576).


%% The encode functions will generate a binary that encodes the
%% requested message. 


encode_response(Header) ->
    Status = "ICY 200 OK\r\n",
    MetaInt = "icy-metaint: " ++ integer_to_list(?Chunk) ++ "\r\n",
    Reply = Status ++ MetaInt ++ encode_header(Header),
    list_to_binary(Reply).

%% Encoding headers, represented by a list of key-value tuples where
%% the value is a string; [[key, "value"}].

encode_header([]) ->
    "\r\n";
encode_header([{Name, Arg}|Rest]) ->
    Str = atom_to_list(Name),
    Str ++ ":" ++ Arg ++ "\r\n" ++ encode_header(Rest).

%% Meta data, a string, needs to be padded to align to 16 bytes. The
%% meta data is typically the title: "This is the song", 

encode_meta(Headers) ->
    Meta = encode_meta_headers(Headers),
    {K, Padded} = padding(Meta),
    <<K/integer, Padded/binary>>.    

encode_meta_headers([]) ->
    [];
encode_meta_headers([{title, Arg}|Rest]) ->
    "StreamTitle='" ++ Arg ++ "';" ++ encode_meta_headers(Rest).


padding(Meta) ->
    N = length(Meta),
    R = (N rem 16),
    MetaBin = list_to_binary(Meta),
    if 
	R == 0 -> 
	    %% This also work for Meta == "" 
	    {N div 16, MetaBin};
	true -> 
	    {(N div 16)+1, <<MetaBin/binary, 0:(8*(16-R))>>}
    end.




%% The decode_request/1 function parses a request and can return either:
%%           {ok, Header, Rest} if a proper request was found
%%           {error, Error} if an error occured
%%           {more, Fun} if more data is needed
%% 

decode_request(Segment) ->
    case request_line(Segment) of 
	{ok, Method, Resource, Version, R1} ->
	    io:format("icy: headers ~s~n", [binary_to_list(R1)]),
	    case decode_headers(R1, []) of
		{ok, Headers, Body} ->
		    {ok, {Method, Resource, Headers, Version}, Body};
		more ->
		    {more, fun(More) -> decode_request(<<Segment/binary, More/binary>>) end}
	    end;
	more ->
	    {more, fun(More) -> decode_request(<<Segment/binary, More/binary>>) end};	    
	{error, Reason} ->
	    {error, "invalid request: " ++ Reason}
    end.


%% Start Line

request_line(Segment) when size(Segment) < 4 ->
    more;
request_line(<<$G, $E, $T, 32, R1/binary>>)	->
    case decode_resource(R1) of
	{ok, Resource, R2} ->
	   case decode_version(R2) of 
	       {ok, Version, R3} ->
		   {ok, get, Resource, Version, R3};
	       more ->
		   more;
	       Other ->
		   {error, "strange version: " ++ Other}
	   end;
	more ->
	    more;
	error ->
	    {error, "failed to parse resource"}
    end;
request_line(Line) ->
    {error, "not a get request: " ++ Line}.


%% Version

decode_version(Segment) when size(Segment) <  10 ->
    more;
decode_version(<<"HTTP/1.0", 13, 10, Rest/binary>>) ->
		   {ok, v10, Rest};
decode_version(<<"HTTP/1.1", 13, 10, Rest/binary>>) ->
		   {ok, v11, Rest}.




%% Resource 

decode_resource(<<>>) ->
    more;
decode_resource(<<32,Rest/binary>>) ->
    {ok, [], Rest};
decode_resource(<<C, Rest/binary>>) ->
    {ok, Resource, Cont} = decode_resource(Rest),
    {ok, [C|Resource], Cont}.
     


%% Headers 

decode_headers(Segment, _) when size(Segment) < 2 ->
    more;
decode_headers(<<13,10,Rest/binary>>, Sofar) ->
    {ok, Sofar, Rest};
decode_headers(Headers, Sofar) ->
    decode_header(Headers, [], Sofar).

decode_header(Segment, _Last, _Sofar) when size(Segment) < 2 ->
    more;
decode_header(<<13,10,Rest/binary>>, Last, Sofar) ->
    Line = lists:reverse(Last),
    {Name, [58|Arg]} = lists:splitwith(fun(X) -> X =/= 58 end, Line),    
    decode_headers(Rest, [{list_to_atom(Name), Arg}|Sofar]);
decode_header(<<C,Rest/binary>>, Last, Sofar) ->
    decode_header(Rest, [C|Last], Sofar).



    
%% Dividing data into chunks of the right size

segments(<<Chunk:?Chunk/binary, Rest/binary>>) ->
    [{seg, Chunk}| segments(Rest)];
segments(_) ->
    [].
    

