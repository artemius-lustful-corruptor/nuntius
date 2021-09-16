-module(chatroom).
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
    %%{messages, []}
    {ok, []}.

%% Check to see if a user name/server pair is unique;
%% if so, add it to the server's state
handle_call({login, {UserName,ChatRoom}, ServerRef}, From, State) ->

    {FromPid, _FromTag} = From,
    case lists:keymember({{UserName,ChatRoom}, ServerRef}, 1, State) of
        true ->
            NewState = State,
            Reply = {error, "User " ++ UserName ++ " already in use."};
        false ->
            NewState = [{{{UserName, ChatRoom},  ServerRef}, FromPid} | State],
            Reply = {ok, "Logged in."}
    end,
    {reply, Reply, NewState};


%% Log out the person sending the message, but only
%% if they're logged in already.
handle_call({logout, Lobby}, From, State) ->
    {FromPid, _FromServer} = From,
    case lists:keymember(FromPid, 2, State) of
        true ->
            {value, {{{UserName, _}, SenderServer}, _}} = lists:keysearch(FromPid, 2, State),
            NewState = lists:keydelete({{UserName, Lobby}, SenderServer}, 1, State),
            Reply  = {ok, logged_out};
        false ->
            NewState = State,
            Reply = {error, not_logged_in}
    end,
    {reply, Reply, NewState};

%% FIXME copied code
handle_call(exit, From, State) ->
    {FromPid, _FromTag} = From,
    {ok, NewState} = delete(State, FromPid),
    Reply = {ok, exited},
    io:format("~p ~n", [NewState]),
    {reply, Reply, NewState};    

%% When receiving a message from a person, use the From PID to
%% get the user's name and server name from the chatroom server state.
%% Send the message via a "cast" to everyone who is NOT the sender.
handle_call({say, SenderLobby,  Text}, From, State) ->
    {FromPid, _FromTag} = From,
    case lists:keymember(FromPid, 2, State) of
        true ->
            {SenderName, SenderServer}  = find_sender(SenderLobby, FromPid, State),
            % For debugging: get the list of recipients.
            %RecipientList = [{{RecipientName, RecipientLobby},  RecipientServer} || 
            %{{{RecipientName, RecipientLobby}, RecipientServer}, _} <- State, 
            %{{RecipientName, RecipientLobby}, RecipientServer} /= {{SenderName, SenderLobby}, SenderServer}], 
            %io:format("Recipient list: ~p~n", [RecipientList]),
            Msg = {message, {SenderName, SenderServer}, Text},
            [gen_server:cast({person, RecipientServer}, Msg) || {{{RecipientName, RecipientLobby}, RecipientServer}, _} <- State, 
            (RecipientName /= SenderName) and (RecipientLobby == SenderLobby)],
            %{value, {messages, MsgList}} = lists:keysearch(messages, 1, State),
            %NewState = [{messages, [{SenderLobby, {SenderName, SenderServer}, Text} | MsgList]} | State];
            NewState = State;
        false -> ok,
            NewState = State
    end,
    {reply, ok, NewState};

%% Get the state of another person and return it to the asker
handle_call({who, Person, ServerRef}, _From, State) ->
                                                % Find pid of the person at the serverref
    Found = lists:keyfind({Person, ServerRef}, 1, State),

    case Found of
        {{_FromUser, _FromServer}, Pid} ->
            Reply = gen_server:call(Pid, get_profile);
        _ ->
            Reply = "Cannot find that user"
    end,
    {reply, Reply, State};


%% Return a list of all users currently in the chat room
handle_call({users, ChatRoom}, _From, State) ->
    UserList = [{{UserName, Lobby}, UserServer} ||
                   {{{UserName, Lobby}, UserServer}, _} <- State, Lobby == ChatRoom], 
    {reply, UserList, State};

%% Return a list of all users
handle_call(users, _From, State) ->
    UserList = [{UserName, UserServer} ||
                   {{{UserName, _Lobby}, UserServer}, _} <- State], 
    {reply, UserList, State};

handle_call({history, Lobby}, _From, State) ->
    {value, {messages, MsgsTuple}} = lists:keysearch(messages, 1, State),
    {value, MsgsList} = lists:keysearch(Lobby, 1, MsgsTuple),
    RetList = lists:sublist(MsgsList, 1, 50),
    {reply, {messages, RetList}, State};

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
    io:format("~p~n", [State]),
    case lists:filter(fun({{{_,L}, _}, Pid}) -> (L == Lobby) and (Pid == FromPid) end, State) of
        [{{{SenderName, _Lobby}, SenderServer}, _}] -> 
            {SenderName, SenderServer};
        [] -> 
            {value, {{{SenderName, _}, SenderServer}, _}} = lists:keysearch(FromPid, 2, State),
            {SenderName, SenderServer}
    end.


