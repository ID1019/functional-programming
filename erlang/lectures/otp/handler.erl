-module(handler).


start_link(Port) ->
    gen_server:start_link(?MODULE, Port, []).


init(Port) ->
    {ok, Listen} = gen_server:call(Port, request),
    {ok, Socket} = gen_tcp:accept(Listen),
    {ok, Socket}.

handle_cast(Msg, )
    
