-module(hecate_app_snake_dueld_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 10, period => 60},
    ChildSpecs = [
        #{
            id => duel_proc_sup,
            start => {duel_proc_sup, start_link, []},
            restart => permanent,
            type => supervisor
        },
        #{
            id => query_snake_duel_store,
            start => {query_snake_duel_store, start_link, []},
            restart => permanent,
            type => worker
        },
        #{
            id => app_snake_dueld_plugin_registrar,
            start => {app_snake_dueld_plugin_registrar, start_link, []},
            restart => transient,
            type => worker
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
