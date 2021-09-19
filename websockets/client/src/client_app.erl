%%%-------------------------------------------------------------------
%% @doc client public API
%% @end
%%%-------------------------------------------------------------------

-module(client_app).
-behaviour(application).

-export([start/2, stop/1, connect/0]).

start(_StartType, _StartArgs) ->
    client_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
connect() ->
    {ok, _} = application:ensure_all_started(gun),
    {ok, ConnPid} = gun:open("localhost", 8080),
    {ok, _Protocol} = gun:await_up(ConnPid),
    gun:ws_upgrade(ConnPid, "/websocket"),
    receive
    {gun_upgrade, ConnPid, _StreamRef, [<<"websocket">>], Headers} ->
        upgrade_success(ConnPid, Headers)
    after 5000 ->
        error(timeout)
    end.



upgrade_success(ConnPid, Headers) ->
    io:format("Upgraded ~w. Success!~nHeaders:~n~p~n", 
              [ConnPid, Headers]),

    gun:ws_send(ConnPid, {text, "It's raining!"}),

    receive
        {gun_ws, ConnPid, {text, Msg} } ->
            io:format("~s~n", [Msg])
    end.
