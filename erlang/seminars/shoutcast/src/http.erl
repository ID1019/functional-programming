-module(http).

-export([test/0, parse_get/1, request_line/1]).

test() ->
    parse_get("GET /foo HTTP/1.1\r\nfoo: 17\r\nbar: 42\r\n\r\nThis is the body").

parse_get(Request) ->
    {ok, Method, Resource, Version, R1} = request_line(Request),
    {ok, Headers, Body} = headers(R1),
    {ok, Method, Resource, Version, Headers, Body}.
    

headers([13,10|Rest]) ->
    {ok, [], Rest};
headers(Headers) ->
    {ok, Header, R1} = header(Headers),
    {ok, Rest, R2} = headers(R1),
    {ok, [Header|Rest], R2}.


header([13,10|Rest]) ->
    {ok, [], Rest};
header([C|Rest]) ->
    {ok, Header, R1} = header(Rest),
    {ok, [C|Header], R1}.


request_line([$G,$E,$T, 32 | R0]) ->
    {Resource, R1} = resource(R0),
    {Version, R2} = version(R1),
    [13,10|R3] = R2,
    {ok, get, Resource, Version, R3}.

resource([32|Rest]) ->
    {[], Rest};
resource([C|Rest]) ->
    {Resource, Cont} = resource(Rest),
    {[C|Resource], Cont}.


%% NOT TRUE but ok, general form is HTTP/12.34 etc

version([$H,$T,$T,$P, $/, $1, $., $2 | Rest]) ->
    {version12, Rest};
version([$H,$T,$T,$P, $/, $1, $., $1 | Rest]) ->
    {version11, Rest};
version([$H,$T,$T,$P, $/, $1, $., $0 | Rest]) ->
    {version10, Rest}.




