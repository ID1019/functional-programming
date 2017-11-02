-module(mp3).

-export([read_file/1, file_id3_tag/1, trim/1]).

read_file(File) ->
    Size = filelib:file_size(File),
    {ok, S} = file:open(File, [read, binary, raw]),
    {ok, Id3, End} = id3_tag(S, Size),
    {ok, Data} = file:pread(S, 0, End),
    Title = id3_title(Id3),
    {mp3, Title, Data}.


id3_title({id3, Tags}) ->
    case lists:keysearch(title, 1, Tags) of
	{value, {title, Title}} ->
	    Title;
	false ->
	    "No title"
    end.	    


file_id3_tag(File) ->
    Size = filelib:file_size(File),
    {ok, S} = file:open(File, [read, binary, raw]),
    id3_tag(S, Size).


%%  ID3 tag, if it exists, is the last 128 bytes of the file.

id3_tag(S, Size) ->
    {ok, Tail} = file:pread(S, Size-128, 128),
    case parse_id3_tag(Tail) of
	na ->
	    {ok, {id3, []}, Size};
	{ok, Id3} ->
	    {ok, Id3, Size-128}
    end.


parse_id3_tag(<<$T, $A, $G, 
	       Title:30/binary, 
	       Artist:30/binary,
	       Album:30/binary,
	       Year:4/binary,
               Comment:28/binary,
	       0,                   % delimiter in v1.1
	       Track:1/binary,      % the track 
               Genre:1/binary>>) ->
    {ok, {id3, [{title, trim(binary_to_list(Title))}, 
		{artist, trim(binary_to_list(Artist))}, 
		{album, trim(binary_to_list(Album))}, 
		{year, binary_to_list(Year)}, 
		{comment, trim(binary_to_list(Comment))}, 
		{track, binary_to_list(Track)}, 
		{genre, binary_to_list(Genre)}]}};
parse_id3_tag(_Tail) ->
    na.
    

trim([]) ->
    [];
trim([0|_]) ->
    [];
trim([C|Rest]) ->
    [C|trim(Rest)].


    

