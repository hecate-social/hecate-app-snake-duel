-module(run_snake_duel_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ok = ensure_pg(),
    run_snake_duel_sup:start_link().

stop(_State) ->
    ok.

ensure_pg() ->
    case pg:start(pg) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end.
