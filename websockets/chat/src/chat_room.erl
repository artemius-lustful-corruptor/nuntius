-module(chat_room).
-behaviour(gen_server).
-export([start_link/0]). % convenience call for startup
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]). % gen_server callbacks

-define(SERVER, ?MODULE). % macro that defines this module as the server

%% The server state consists of a list of tuples for each person in chat.
%% Each tuple has the format {{UserName, UserServer}, PID of person}
%%% convenience method for startup
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%% gen_server callbacks
init([]) ->
    
    io:format("Chat server started. ~n"),
    {ok, [{users, []},{messages, []}]}.

%% Check to see if a user name/server pair is unique;
%% if so, add it to the server's state
handle_call({login, {UserName,ChatRoom}, ServerRef}, From, State) ->

    {FromPid, _FromTag} = From,
    {value, {users, UsersTuple}} = lists:keysearch(users, 1, State),
    case lists:keymember({{UserName,ChatRoom}, ServerRef}, 1, UsersTuple) of
        true ->
            NewUsers = UsersTuple,
            Reply = {error, "User " ++ UserName ++ " already in use."};
        false ->
            NewUsers = [{{{UserName, ChatRoom},  ServerRef}, FromPid} | UsersTuple],
            Reply = {ok, "Logged in", ChatRoom}
    end,
    NewState = lists:keyreplace(users, 1, State, {users, NewUsers}),
    {reply, Reply, NewState};


%% Log out the person sending the message, but only
%% if they're logged in already.
handle_call({logout, Lobby}, From, State) ->
    {FromPid, _FromServer} = From,
    {value, {users, UsersTuple}} = lists:keysearch(users, 1, State),
    case lists:keymember(FromPid, 2, UsersTuple) of
        true ->
            {value, {{{UserName, _}, SenderServer}, _}} = lists:keysearch(FromPid, 2, UsersTuple),
            NewUsers = lists:keydelete({{UserName, Lobby}, SenderServer}, 1, UsersTuple),
            Reply  = {ok, logged_out};
        false ->
            NewUsers = State,
            Reply = {error, not_logged_in}
    end,
    NewState = lists:keyreplace(users, 1, State, {users, NewUsers}),
    {reply, Reply, NewState};

%% FIXME copied code
handle_call(exit, From, State) ->
    {FromPid, _FromTag} = From,
    {value, {users, UsersTuple}} = lists:keysearch(users, 1, State),
    {ok, NewUsers} = delete(UsersTuple, FromPid),
    Reply = {ok, exited},
    io:format("~p ~n", [NewUsers]),
    NewState = lists:keyreplace(users, 1, State, {users, NewUsers}),
    {reply, Reply, NewState};    

%% When receiving a message from a person, use the From PID to
%% get the user's name and server name from the chatroom server state.
%% Send the message via a "cast" to everyone who is NOT the sender.
handle_call({say, SenderLobby,  Text}, From, State) ->
    {FromPid, _FromTag} = From,
    {value, {users, UsersTuple}} = lists:keysearch(users, 1, State),
    case lists:keymember(FromPid, 2, UsersTuple) of
        true ->
            {SenderName, SenderServer}  = find_sender(SenderLobby, FromPid, UsersTuple),
            Msg = {{SenderName, SenderLobby}, Text},
            [RecipientServer ! {message, Msg} || {{{RecipientName, RecipientLobby}, RecipientServer}, _} <- UsersTuple, 
            (RecipientName /= SenderName) and (RecipientLobby == SenderLobby)],
            {value, {messages, MsgsTuple}} = lists:keysearch(messages, 1, State),
            case lists:keysearch(SenderLobby, 1, MsgsTuple) of
                false -> 
                    HistoryMsg = {SenderLobby, [{{SenderName, SenderServer}, Text}]},
                    NewMsgsTuple = [HistoryMsg | MsgsTuple];
                 {value, {SenderLobby, MsgsList}} -> 
                    HistoryMsgs = [{{SenderName, SenderServer}, Text} | MsgsList],
                    NewMsgsTuple = lists:keyreplace(SenderLobby, 1, MsgsTuple, {SenderLobby, HistoryMsgs})
            end,
            NewState = lists:keyreplace(messages, 1, State, {messages, NewMsgsTuple});
        false -> ok,
            NewState = State
    end,
    {reply, ok, NewState};


%% Return a list of all users currently in the chat room
handle_call({users, ChatRoom}, _From, State) ->
    {value, {users, UsersTuple}} = lists:keysearch(users, 1, State),
    UserList = [{{UserName, Lobby}, UserServer} ||
                   {{{UserName, Lobby}, UserServer}, _} <- UsersTuple, Lobby == ChatRoom], 
    {reply, UserList, State};

%% Return a list of all users
handle_call(users, _From, State) ->
    {value, {users, UsersTuple}} = lists:keysearch(users, 1, State),
    UserList = [{UserName, UserServer} ||
                   {{{UserName, _Lobby}, UserServer}, _} <- UsersTuple], 
    {reply, UserList, State};

handle_call({history, Lobby}, _From, State) ->
    {value, {messages, MsgsTuple}} = lists:keysearch(messages, 1, State),
    case lists:keysearch(Lobby, 1, MsgsTuple) of 
        {value, {_, MsgsList}}  -> RetList = lists:sublist(MsgsList, 1, 50);
        _ -> RetList = []
    end,
    {reply, {messages, RetList}, State};

handle_call(lobbies, _From, State) ->
    Res = 
        case  file:consult("lobbies.txt") of
            {ok, Terms} -> Terms; 
            _ -> []
        end,
    {reply, Res, State};

handle_call(Request, _From, State) ->
    {ok, {error, "Unhandled Request", Request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    io:format("Received unknown message ~p~n", [Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% HELPERS
delete(State, FromPid) ->
    case lists:keymember(FromPid, 2, State) of
        true ->
            NewState = lists:keydelete(FromPid, 2, State),
            delete(NewState, FromPid);

        false ->
            {ok, State}
    end.

find_sender(Lobby, FromPid, State) ->
    FilterRes = lists:filter(fun({{{_,L}, _}, Pid}) -> (L == Lobby) and (Pid == FromPid) end, State),
    io:format("Senders list: ~p~n", [FilterRes]),
    case FilterRes of
        [{{{SenderName, _Lobby}, SenderServer}, _}] -> 
            {SenderName, SenderServer};
        [] -> 
            {value, {{{SenderName, _}, SenderServer}, _}} = lists:keysearch(FromPid, 2, State),
            {SenderName, SenderServer}
    end.

