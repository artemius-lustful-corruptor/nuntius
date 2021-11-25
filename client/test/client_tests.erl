-module(client_tests).
   -include_lib("eunit/include/eunit.hrl").

chat_client_test() ->
    ChatRoom = "Cats",
    Response = {ok, "Logged in", ChatRoom},
    Res = chat_handler:login("Nick", ChatRoom, test),
    ?assertEqual(Response, Res).
