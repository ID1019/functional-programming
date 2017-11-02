-module(super).


-behaeviour(supervisor).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Port = {port, {port, start_link, [8080]}, temporary, 2000, worker, [port]},
    Handlers = {handlers, {handlers, start_link, [port]}, temporary, 2000, worker, [port]},
    {ok, {one_for_rest, 1, 1000}, [Port, Handlers]}.
