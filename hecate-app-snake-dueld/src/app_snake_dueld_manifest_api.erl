-module(app_snake_dueld_manifest_api).
-export([init/2]).

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> ->
            {ok, Vsn} = application:get_key(hecate_app_snake_dueld, vsn),
            Body = json:encode(#{
                <<"name">> => <<"snake-duel">>,
                <<"display_name">> => <<"Snake Duel">>,
                <<"version">> => list_to_binary(Vsn),
                <<"icon">> => <<16#1F40D/utf8>>,
                <<"description">> => <<"Snake Duel Arena">>,
                <<"tag">> => <<"snake-duel-studio">>
            }),
            Req1 = cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>,
                <<"access-control-allow-origin">> => <<"*">>
            }, Body, Req0),
            {ok, Req1, State};
        _ ->
            Req1 = cowboy_req:reply(405, #{
                <<"content-type">> => <<"application/json">>
            }, <<"{\"ok\":false,\"error\":\"method_not_allowed\"}">>, Req0),
            {ok, Req1, State}
    end.
