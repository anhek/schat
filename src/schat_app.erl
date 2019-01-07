-module(schat_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    ok = schat_user_mng:init(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws", schat_ws, []}
        ]}
    ]),
    Port = application:get_env(schat, port, 8080),
    {ok, _} = cowboy:start_clear(schat, [{port, Port}], #{
        env => #{dispatch => Dispatch}
    }),
    schat_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(schat).
