-module(chat_handler).
-behaviour(gen_server).
-export([start_link/0]). % convenience call for startup
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]). % gen_server callbacks

-record(state, {chat_conn, chat_ref, chat_room, chat_rooms}).

-export([login/2, 
         logout/1, 
         exit/0, 
         say/2, 
         users/1, 
         users/0, 
         lobbies/0,  
         history/1]). 


-define(CLIENT, ?MODULE). % macro that defines this module as the client

%% API
%% @doc Login to a server using a name
%% If you connect, tell the server your user name and node.
%% You don't need a reply from the server for this.
-spec(login(string(), string()) -> term()).
login(UserName, ChatRoom) ->
    if %re-write this
        is_atom(UserName) ->
            gen_server:call(?CLIENT,
                            {login, atom_to_list(UserName), atom_to_list(ChatRoom)});
        is_list(UserName) ->
            gen_server:call(?CLIENT,
                            {login, UserName, ChatRoom});
        true ->
            {error, "User name must be an atom or a list"}
    end.


%% @doc Log out of the system. The person server will send a From that tells
%% who is logging out; the chatroom server doesn't need to reply.
-spec(logout(string()) -> atom()).
logout(Lobby) ->
    gen_server:call(?CLIENT, {logout, Lobby}).
%% @doc Exit from the server
-spec(exit() -> atom()).
exit() ->
    gen_server:call(?CLIENT, exit).

%% @doc Send the given Text to the chat room server. No reply needed.
-spec(say(string(), string()) -> atom()).
say(Lobby, Text) ->
    gen_server:call(?CLIENT, {say, Lobby, Text}).

%% @doc Ask the server for a list of chat room's users.
-spec(users(string()) -> [string()]).
users(Lobby) ->
    gen_server:call(?CLIENT, {users, Lobby}).

%% @doc Ask the server for a list of users.
-spec(users() -> [string()]).
users() ->
    gen_server:call(?CLIENT, users).

%% @doc Ask the server for a list of chat rooms
-spec(lobbies() -> [tuple()]).
lobbies() ->
    self() ! lobbies. 

%% @doc Get history of the last 50 messages
-spec(history(string()) -> [tuple()]).
history(Lobby) ->
    gen_server:cast(?CLIENT, {history, Lobby}).



%% CALLBACKS
%%% convenience method for startup
start_link() ->
    gen_server:start_link({local, ?CLIENT}, ?MODULE, [], []).

init([])->
    timer:send_after(2000, lobbies),
    timer:send_after(1000, connect),
    
    {ok, #state{chat_rooms=[]}}.

handle_call({login, UserName, ChatRoom}, _From, State) ->
    Reply =
        case get_chat_room(ChatRoom, State) of
            {ok, continue} -> 
                Bin = erlang:term_to_binary({login, {UserName, ChatRoom}}),
                gun:ws_send(State#state.chat_conn, State#state.chat_ref, {binary, Bin});
            Error -> 
                Error
        end, 
    {reply, Reply, State};

handle_call({logout, Lobby}, _From, State) ->
    Reply = 
        case get_chat_room(Lobby, State) of
            {ok, continue} -> 
                Bin = erlang:term_to_binary({logout, Lobby}),
                gun:ws_send(State#state.chat_conn, State#state.chat_ref, {binary, Bin});
            Error -> 
                Error
    end,
    {reply, Reply, State};

handle_call(exit, _From, State) ->
    Bin = erlang:term_to_binary(exit),
    Reply = gun:ws_send(State#state.chat_conn, State#state.chat_ref, {binary, Bin}),
    {reply, Reply, State};

handle_call({users, Lobby}, _From, State) ->
    Bin = erlang:term_to_binary({users, Lobby}),
    Reply = gun:ws_send(State#state.chat_conn, State#state.chat_ref, {binary, Bin}),
    {reply, Reply, State};

handle_call(users, _From, State) ->
    Bin = erlang:term_to_binary(users),
    Reply = gun:ws_send(State#state.chat_conn, State#state.chat_ref, {binary, Bin}),
    {reply, Reply, State};

handle_call({say, Lobby, Text}, _From, State) ->
    Reply =
        case get_chat_room(Lobby, State) of
            {ok, continue} -> 
                Bin = erlang:term_to_binary({say, Lobby, Text}),
                gun:ws_send(State#state.chat_conn, State#state.chat_ref, {binary, Bin});
            Error -> Error
        end,  
    {reply, Reply, State}.

handle_cast({history, Lobby}, State) ->
    case get_chat_room(Lobby, State) of
        {ok, continue} -> 
            Bin = erlang:term_to_binary({history, Lobby}),
            gun:ws_send(State#state.chat_conn, State#state.chat_ref, {binary, Bin});
        Error -> 
            io:format("Error: ~p~n", [Error])
        end,    
    {noreply, State};

handle_cast(_Request, State) ->
    io:format("Unknown request ~p~n", _Request),
    {noreply, State}.

handle_info(connect, State) ->
   {ConnPid, Ref} = connect(),
   {noreply, State#state{chat_conn=ConnPid, chat_ref=Ref}};

handle_info({gun_ws, _ConnPid, _Ref, {binary, Data}}, State) ->
    Term = erlang:binary_to_term(Data),
    io:format("~p~n", [Term]),
    NewState = 
        case Term of
            {lobbies, Lobbies} -> State#state{chat_rooms=Lobbies};
            {ok,"Logged in", Lobby} -> State#state{chat_room=Lobby};
            _ -> State
        end,
    {noreply, NewState};

handle_info(lobbies, State) ->
    Bin = erlang:term_to_binary(lobbies),
    gun:ws_send(State#state.chat_conn, State#state.chat_ref, {binary, Bin}),
    {noreply, State};

handle_info(Info, State) ->
    io:format("~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Helpers
connect() ->
    io:format("Connecting..~n"),
    {ok, ConnPid} = gun:open("localhost", 8080),
    gun:ws_upgrade(ConnPid, "/websocket"),
  receive
    {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers} ->
        upgrade_success(ConnPid, StreamRef);
    {gun_response, ConnPid, _, _, Status, Headers} ->
        exit({ws_upgrade_failed, Status, Headers});
    {gun_error, ConnPid, _StreamRef, Reason} ->
        exit({ws_upgrade_failed, Reason})
    %% More clauses here as needed.
after 1000 ->
    exit(timeout)
end.

upgrade_success(ConnPid, Ref) ->
    io:format("Upgraded ~w. Success!~n", [ConnPid]),
    {ConnPid, Ref}.
    
get_chat_room(Lobby, State)  ->
    case lists:keysearch(Lobby, 2, State#state.chat_rooms) of
        {value, _} -> {ok, continue};
        _ ->  {error, "That chat room unavailable"}
    end.
%get_chat_room(_Lobby, _State) ->
%    {error, "Access forbiden"}.
