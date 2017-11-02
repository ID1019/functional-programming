-module(handlers).

-behaviour(supervisor).


start_link(Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Port).

init(Port) ->
    H1 = {handler1, {handler, start_link, [Port]}, temporary, 2000, worker, [handler]},
    H2 = {handler1, {handler, start_link, [Port]}, temporary, 2000, worker, [handler]},
    H3 = {handler1, {handler, start_link, [Port]}, temporary, 2000, worker, [handler]},
    {ok, {one_for_one, 1, 1000}, [H1, H2, H3]}.


