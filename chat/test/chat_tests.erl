-module(chat_tests).
   -include_lib("eunit/include/eunit.hrl").

chat_room_server_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun server_is_alive/1,
        fun login/1
    ]}.
 
server_is_alive({Pid, ClientPid}) ->
    fun() ->
        ?assertEqual(true, is_process_alive(Pid))
    end.
 
login({Pid, ClientPid}) ->
    fun() ->
        ChatRoom = "Cats",
        Params = {"Nick", ChatRoom},
        Response = {ok, "Logged in", ChatRoom},
        ?assertEqual(Response, gen_server:call(Pid, {login, Params, ClientPid}))
    end.
 
setup() ->
    ?debugMsg("setup"),
    process_flag(trap_exit, true),
    {ok, Pid} = chat_room:start_link(),
    ClientPid = "<0.39.0>",
    {Pid, ClientPid}.
 
cleanup({Pid, ClientPid}) ->
    ?debugMsg("cleanup"),
    exit(Pid, kill), %% brutal kill!
    ?assertEqual(false, is_process_alive(Pid)).

