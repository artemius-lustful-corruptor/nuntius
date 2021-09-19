-module(chat_handler).
-export([init/2,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2]).
         %terminate/2]).

-define(SERVER, ?MODULE). % macro that defines this module as the server

%% The server state consists of a list of tuples for each person in chat.
%% Each tuple has the format {{UserName, UserServer}, PID of person}
%%% convenience method for startup
%start_link() ->
%    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%% gen_server callbacks
init(Req, _State) ->
    io:format("Init: ~p~n", [Req]),
    {cowboy_websocket, Req, [{users, []},{messages, []}]}.


websocket_init(State) ->
	erlang:start_timer(1000, self(), <<"Hello!">>),
	{[], State}.


%% Check to see if a user name/server pair is unique;
%% if so, add it to the server's state
websocket_handle({login, Text}, State) ->
    io:format("Hello ~p~n", [Text]),
    %% {FromPid, _FromTag} = From,
    %% {value, {users, UsersTuple}} = lists:keysearch(users, 1, State),
    %% {NewUsers, Reply} = case lists:keymember({{UserName,ChatRoom}, ServerRef}, 1, UsersTuple) of
    %%     true ->
    %%         {UsersTuple,{error, "User " ++ UserName ++ " already in use."}};
    %%     false ->
    %%         {[{{{UserName, ChatRoom},  ServerRef}, FromPid} | UsersTuple], {ok, "Logged in."}}
    %% end,
    %% NewState = lists:keyreplace(users, 1, State, {users, NewUsers}),
    {ok, State};

websocket_handle(Data, State) ->
    io:format("~p~n", [Data]),
    {[], State}.


%% %% Log out the person sending the message, but only
%% %% if they're logged in already.
%% websocket_handle({logout, Lobby}, From, State) ->
%%     {FromPid, _FromServer} = From,
%%     {value, {users, UsersTuple}} = lists:keysearch(users, 1, State),
%%     case lists:keymember(FromPid, 2, UsersTuple) of
%%         true ->
%%             {value, {{{UserName, _}, SenderServer}, _}} = lists:keysearch(FromPid, 2, UsersTuple),
%%             NewUsers = lists:keydelete({{UserName, Lobby}, SenderServer}, 1, UsersTuple),
%%             Reply  = {ok, logged_out};
%%         false ->
%%             NewUsers = State,
%%             Reply = {error, not_logged_in}
%%     end,
%%     NewState = lists:keyreplace(users, 1, State, {users, NewUsers}),
%%     {reply, Reply, NewState};

%% %% FIXME copied code
%% websocket_handle(exit, From, State) ->
%%     {FromPid, _FromTag} = From,
%%     {value, {users, UsersTuple}} = lists:keysearch(users, 1, State),
%%     {ok, NewUsers} = delete(UsersTuple, FromPid),
%%     Reply = {ok, exited},
%%     io:format("~p ~n", [NewUsers]),
%%     NewState = lists:keyreplace(users, 1, State, {users, NewUsers}),
%%     {reply, Reply, NewState};    

%% %% When receiving a message from a person, use the From PID to
%% %% get the user's name and server name from the chatroom server state.
%% %% Send the message via a "cast" to everyone who is NOT the sender.
%% websocket_handle({say, SenderLobby,  Text}, From, State) ->
%%     {FromPid, _FromTag} = From,
%%     {value, {users, UsersTuple}} = lists:keysearch(users, 1, State),
%%     case lists:keymember(FromPid, 2, UsersTuple) of
%%         true ->
%%             {SenderName, SenderServer}  = find_sender(SenderLobby, FromPid, UsersTuple),
%%             % For debugging: get the list of recipients.
%%             %RecipientList = [{{RecipientName, RecipientLobby},  RecipientServer} || 
%%             %{{{RecipientName, RecipientLobby}, RecipientServer}, _} <- UsersTuple, 
%%             %{{RecipientName, RecipientLobby}, RecipientServer} /= {{SenderName, SenderLobby}, SenderServer}], 
%%             %io:format("Recipient list: ~p~n", [RecipientList]),
%%             Msg = {message, {SenderName, SenderLobby}, Text},
%%             [gen_server:cast({person, RecipientServer}, Msg) || {{{RecipientName, RecipientLobby}, RecipientServer}, _} <- UsersTuple, 
%%             (RecipientName /= SenderName) and (RecipientLobby == SenderLobby)],
%%             {value, {messages, MsgsTuple}} = lists:keysearch(messages, 1, State),
%%             case lists:keysearch(SenderLobby, 1, MsgsTuple) of
%%                 false -> 
%%                     HistoryMsg = {SenderLobby, [{{SenderName, SenderServer}, Text}]},
%%                     NewMsgsTuple = [HistoryMsg | MsgsTuple];
%%                  {value, {SenderLobby, MsgsList}} -> 
%%                     HistoryMsgs = [{{SenderName, SenderServer}, Text} | MsgsList],
%%                     NewMsgsTuple = lists:keyreplace(SenderLobby, 1, MsgsTuple, {SenderLobby, HistoryMsgs})
%%             end,
%%             NewState = lists:keyreplace(messages, 1, State, {messages, NewMsgsTuple});
%%         false -> ok,
%%             NewState = State
%%     end,
%%     {reply, ok, NewState};

%% %% Get the state of another person and return it to the asker
%% websocket_handle({who, Person, ServerRef}, _From, State) ->
%%                                                  % Find pid of the person at the serverref
%%     Found = lists:keyfind({Person, ServerRef}, 1, State),

%%     case Found of
%%         {{_FromUser, _FromServer}, Pid} ->
%%             Reply = gen_server:call(Pid, get_profile);
%%         _ ->
%%             Reply = "Cannot find that user"
%%     end,
%%     {reply, Reply, State};


%% %% Return a list of all users currently in the chat room
%% websocket_handle({users, ChatRoom}, _From, State) ->
%%     {value, {users, UsersTuple}} = lists:keysearch(users, 1, State),
%%     UserList = [{{UserName, Lobby}, UserServer} ||
%%                    {{{UserName, Lobby}, UserServer}, _} <- UsersTuple, Lobby == ChatRoom], 
%%     {reply, UserList, State};

%% %% Return a list of all users
%% websocket_handle(users, _From, State) ->
%%     {value, {users, UsersTuple}} = lists:keysearch(users, 1, State),
%%     UserList = [{UserName, UserServer} ||
%%                    {{{UserName, _Lobby}, UserServer}, _} <- UsersTuple], 
%%     {reply, UserList, State};

%% websocket_handle({history, Lobby}, _From, State) ->
%%     {value, {messages, MsgsTuple}} = lists:keysearch(messages, 1, State),
%%     case lists:keysearch(Lobby, 1, MsgsTuple) of 
%%         {value, {_, MsgsList}}  -> RetList = lists:sublist(MsgsList, 1, 50);
%%         _ -> RetList = []
%%     end,
%%     {reply, {messages, RetList}, State};

%% websocket_handle(lobbies, _From, State) ->
%%     case  file:consult("lobbies.txt") of
%%         {ok, Terms} -> Res =  Terms; 
%%          _ -> Res = []
%%     end,
%%     {reply, Res, State}.


websocket_info({timeout, _Ref, Msg}, State) ->
	erlang:start_timer(1000, self(), <<"How' you doin'?">>),
	{[{text, Msg}], State};
websocket_info(_Info, State) ->
	{{error, "Unhandled Request"}, State}.

%% terminate(_Reason, _State) ->
%%     ok.

%% %% HELPERS
%% delete(State, FromPid) ->
%%     case lists:keymember(FromPid, 2, State) of
%%         true ->
%%             NewState = lists:keydelete(FromPid, 2, State),
%%             delete(NewState, FromPid);

%%         false ->
%%             {ok, State}
%%     end.

%% find_sender(Lobby, FromPid, State) ->
%%     io:format("~p~n", [State]),
%%     case lists:filter(fun(

%% {{{_,L}, _}, Pid}
%% ) ->
%%  (L == Lobby) and (Pid == FromPid) 
%% end, State) of
%%         [{{{SenderName, _Lobby}, SenderServer}, _}] -> 
%%             {SenderName, SenderServer};
%%         [] -> 
%%             {value, {{{SenderName, _}, SenderServer}, _}} = lists:keysearch(FromPid, 2, State),
%%             {SenderName, SenderServer}
%%     end.


