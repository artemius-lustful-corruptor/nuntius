-module(person).
-behaviour(gen_server).
-export([start_link/1]). % convenience call for startup
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]). % gen_server callbacks

-record(state, {chat_node, profile, chat_rooms}).

% internal functions
-export([login/2, logout/1, exit/0, say/2, users/1, users/0, lobbies/0,  who/2, set_profile/2, history/1]). 

-define(CLIENT, ?MODULE). % macro that defines this module as the client

%%% convenience method for startup
start_link(ChatNode) ->
  gen_server:start_link({local, ?CLIENT}, ?MODULE, ChatNode, []).

init(ChatNode)->
  io:format("Chat node is: ~p~n", [ChatNode]),
  {ok, #state{chat_node=ChatNode, profile=[], chat_rooms=[]}}.


%%TODO add the error handling for chat rooms with function get_chat_room(Lobby)

handle_call(get_chat_node, _From, State) ->
  {reply, State#state.chat_node, State};

handle_call({get_chat_room, Lobby}, _From, State) ->
   case lists:keysearch(Lobby, 1, State#state.chat_rooms) of
       {value, {ChatRoom}} -> Res = ChatRoom;
       _ -> Res = {error, "Not fount"}
   end,  
   {reply, Res, State};

handle_call(get_profile, _From, State) ->
  {reply, State#state.profile, State};

handle_call({set_profile, Key, Value}, _From, State) ->
  case lists:keymember(Key, 1, State#state.profile) of
    true -> NewProfile = lists:keyreplace(Key, 1, State#state.profile,
      {Key, Value});
    false -> NewProfile = [{Key, Value} | State#state.profile]
  end,
  {reply, NewProfile,
    #state{chat_node = State#state.chat_node, profile=NewProfile}};

handle_call({login, UserName, ChatRoom}, _From, State) ->
  Reply = gen_server:call({chatroom, State#state.chat_node}, {login, {UserName, ChatRoom}, node()}),
  {reply, Reply, State#state{chat_rooms=[ChatRoom | State#state.chat_rooms]}};

handle_call({logout, Lobby}, _From, State) ->
  %% FIXME Need to add the errors handling
  Reply = gen_server:call({chatroom, State#state.chat_node}, {logout, Lobby}),
  {reply, Reply, State};



handle_call({say, Lobby, Text}, _From, State) ->
  %% FIXME Need to add the errors handling
  Reply = gen_server:call({chatroom, State#state.chat_node}, {say, Lobby, Text}),
  {reply, Reply, State};



handle_call(exit, _From, State) ->
  Reply = gen_server:call({chatroom, State#state.chat_node}, exit),
  {reply, Reply, State}; 

handle_call(_, _From, State) -> {ok, [], State}.

handle_cast({message, {FromUser, FromChatRoom}, Text}, State) ->
  io:format("~s (~p) says: ~p~n", [FromUser, FromChatRoom, Text]),
  {noreply, State};

handle_cast(_Request, State) ->
  io:format("Unknown request ~p~n", _Request),
  {noReply, State}.

handle_info(Info, State) ->
  io:format("Received unexpected message: ~p~n", [Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% internal functions

%% @doc Gets the name of the chat host. This is a really
%% ugly hack; it works by sending itself a call to retrieve
%% the chat node name from the server state.

get_chat_node() ->
  gen_server:call(person, get_chat_node). %change person to MODULE for interesting

get_chat_room(Lobby) ->
  gen_server:call(person, {get_chat_room, Lobby}).

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
  gen_server:call(?CLIENT, {logout, Lobby}),
  ok.
%% @doc Exit from the server
-spec(exit() -> atom()).
exit() ->
  gen_server:call(?CLIENT, exit),
  ok.  

%% @doc Send the given Text to the chat room server. No reply needed.
-spec(say(string(), string()) -> atom()).
say(Lobby, Text) ->
  gen_server:call(?CLIENT, {say, Lobby, Text}),
  ok.

%% @doc Ask the server for a list of chat room's users.
-spec(users(string()) -> [string()]).
users(Lobby) ->
  gen_server:call({chatroom, get_chat_node()}, {users, Lobby}).

%% @doc Ask the server for a list of users.
-spec(users() -> [string()]).
users() ->
  gen_server:call({chatroom, get_chat_node()}, users).

%% @doc Ask the server for a list of chat rooms
-spec(lobbies() -> [string()]).
lobbies() ->
  gen_server:call({chatroom, get_chat_node()}, lobbies).  

%% @doc Ask chat room server for a profile of a given person.
-spec(who(string(), atom()) -> [tuple()]).
who(Person, ServerRef) ->
  gen_server:call({chatroom, get_chat_node()},
    {who, Person, ServerRef}).

%% @doc Get history of the last 50 messages
-spec(history(string()) -> [tuple()]).
history(Lobby) ->
    gen_server:call({chatroom, get_chat_node()}, {history, Lobby}).

%% @doc Update profile with a key/value pair.
-spec(set_profile(atom(), term()) -> term()).
set_profile(Key, Value) ->
  % ask *this* server for the current state
  NewProfile = gen_server:call(?CLIENT, {set_profile, Key, Value}),
  {ok, NewProfile}.






