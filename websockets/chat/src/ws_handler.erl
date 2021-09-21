-module(ws_handler).
-export([init/2,
         websocket_init/3,
         websocket_handle/2,
         websocket_info/2,
         terminate/2]).

init(Req, _State) ->
    {cowboy_websocket, Req, [], #{idle_timeout => 60 * 1000 * 10} }.


websocket_init(_TransportName, Req, _Opts) ->
    %chat_room:enter(self()),
    io:format("New connection: ~p~n", [self()]),
    {ok, Req, undefined_state}.

websocket_handle(Data, State) ->
    {binary, Bin} = Data,
    Term = erlang:binary_to_term(Bin),
    io:format("Connection: ~p~n", [self()]),
    io:format("~p~n", [Term]),
    Reply = case Term of
                {login, {UserName, ChatRoom}} -> 
                    gen_server:call(chat_room, {login, {UserName, ChatRoom}, self()});
                lobbies ->  
                    {lobbies, gen_server:call(chat_room, lobbies)};
                {logout, Lobby} -> 
                    gen_server:call(chat_room, {logout, Lobby});
                exit -> 
                    gen_server:call(chat_room, exit);
                users -> 
                    gen_server:call(chat_room, users);
                {users, Lobby} -> 
                    gen_server:call(chat_room, {users, Lobby});
                {say, Lobby, Text} ->
                    gen_server:call(chat_room, {say, Lobby, Text});
                {history, Lobby} -> 
                    gen_server:call(chat_room, {history, Lobby});
                 _ -> "Unexpected message"
            end,
    RetBin = erlang:term_to_binary(Reply),
    io:format("~p~n", [Reply]),
    {[{binary, RetBin}], State}.

websocket_info({message, Msg}, State) ->
    RetBin = erlang:term_to_binary(Msg),
    {[{binary, RetBin}],State}.

terminate(_Reason, _State) ->
     ok.


