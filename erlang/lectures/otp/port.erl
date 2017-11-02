-module(port).

%-behaviour(gen_server).

-define(Opt,[{packet, 0}, {reuseaddr, true}, {active, true}, {nodelay, true}]).


start_link(Port) ->
    gen_server:start_link(?MODULE, Port, []).


init(Port) ->
    {ok, Listen} = gen_tcp:listen(Port, ?Opt),
    {ok, Listen}.


handle_call(request, From, Listen) ->
    {ok, Listen, Listen}.


   
    
