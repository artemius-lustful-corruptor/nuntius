# Nuntius
## Simple Erlang chat with websockets and erlang client

## Instructions to start app

1. Open the first terminal to start server
2. Run command ```rebar3 shell```. The server has been started
3. Open the second terminal to start client
4. Run command ```rebar3 shell```. 
    The client has been started. Afeter a few seconds after compile, the client will connect to the server and show lobbies list.
    You can connect multiple clients with other terminals.
5. Now you can login to chat room. For example run the command: ```chat_handler:login("Nick", "cats").``` in client terminal
6. To say somethhing run the command: ```chat_handler:say("cats", "Hello").```
    The server support:
    - login 
    - logout 
    - messaging 
    - showing lobbies

## All PR are welcome
