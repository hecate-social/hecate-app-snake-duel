-module(hecate_app_snake_dueld_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case application:get_env(hecate_app_snake_dueld, enabled, true) of
        false ->
            logger:info("[hecate_app_snake_dueld] Disabled by config"),
            {ok, spawn(fun() -> receive stop -> ok end end)};
        true ->
            ok = app_snake_dueld_paths:ensure_layout(),
            ok = start_cowboy(),
            logger:info("[hecate_app_snake_dueld] Started, socket at ~s",
                        [app_snake_dueld_paths:socket_path("api.sock")]),
            hecate_app_snake_dueld_sup:start_link()
    end.

stop(_State) ->
    ok = cowboy:stop_listener(app_snake_dueld_http),
    cleanup_socket(),
    ok.

start_cowboy() ->
    SocketPath = app_snake_dueld_paths:socket_path("api.sock"),
    cleanup_socket_file(SocketPath),
    Routes = [
        {"/health", app_snake_dueld_health_api, []},
        {"/manifest", app_snake_dueld_manifest_api, []},
        {"/api/arcade/snake-duel/matches", start_duel_api, []},
        {"/api/arcade/snake-duel/matches/:match_id/stream", stream_duel_api, []},
        {"/api/arcade/snake-duel/leaderboard", get_leaderboard_api, []},
        {"/api/arcade/snake-duel/matches/:match_id", get_match_by_id_api, []},
        {"/api/arcade/snake-duel/history", get_match_history_api, []}
    ],
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    TransOpts = #{
        socket_opts => [{ifaddr, {local, SocketPath}}],
        num_acceptors => 5
    },
    ProtoOpts = #{
        env => #{dispatch => Dispatch},
        idle_timeout => 600000,
        request_timeout => 600000
    },
    {ok, _} = cowboy:start_clear(app_snake_dueld_http, TransOpts, ProtoOpts),
    ok.

cleanup_socket() ->
    SocketPath = app_snake_dueld_paths:socket_path("api.sock"),
    cleanup_socket_file(SocketPath).

cleanup_socket_file(Path) ->
    case file:delete(Path) of
        ok -> ok;
        {error, enoent} -> ok;
        {error, Reason} ->
            logger:warning("[hecate_app_snake_dueld] Failed to remove socket ~s: ~p", [Path, Reason]),
            ok
    end.
