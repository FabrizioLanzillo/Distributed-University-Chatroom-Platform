-module(chatroom_websocket).

-export([init/2, websocket_handle/2, websocket_info/2, terminate/3]).
-include("chat.hrl").


% Cowboy will call init/2 whenever a request is received,
% in order to establish a websocket connection.
init(Req, Opts) ->
    % Switch to cowboy_websocket module
    {cowboy_websocket, Req, Opts}.


% Cowboy will call websocket_handle/2 whenever a text, binary, ping or pong frame arrives from the client.
websocket_handle(Frame = {text, _Message}, State) ->
    io:format("websocket_handle(~p, ~p)~n", [Frame, State]),
    io:format("Received ~p~n", [Frame]),
    
    Result = jsone:try_decode(_Message),

    Response = case element(1, Result) of
        ok ->
            Map = element(2, Result),
            handle_websocket_frame(Map, State);
        error ->
            io:format(element(2, Result)),
            {ok, State}
    end,
    Response;

websocket_handle(_Frame, State) ->
    {ok, State}.



% Handle a frame after JSON decoding
handle_websocket_frame(Map, State) ->
    io:format("Map is ~p~n", [Map]),
    {ok, Opcode} = maps:find(<<"opcode">>, Map),

    Response = case Opcode of
        <<"LOGIN">> ->
            handle_login(Map, State);
        <<"UPDATE_ONLINE_USERS">> ->
            handle_update_online_users(Map, State);
        <<"LOGOUT">> ->
            handle_logout(State);
        <<"MESSAGE">> ->
            handle_chat_message(Map, State)
    end,
    Response.



% Handle a login request
handle_login(Map, State) ->
    io:format("login request received~n"),
    {ok, Course} = maps:find(<<"course">>, Map),
    {ok, Username} = maps:find(<<"username">>, Map),
    gen_server:cast(?CHATROOM_SERVER, {login, {self(), Course, binary_to_list(Username)}}),
    {ok, Course}.



% Handle a request for updating online users
handle_update_online_users(Map, State) ->
    io:format("update_online_users request received~n"),
    {ok, Course} = maps:find(<<"course">>, Map),
    Users = gen_server:call(?CHATROOM_SERVER, {update_online_users, {self(), Course}}),
    Message = jsone:encode(
        #{
            <<"opcode">> => <<"UPDATE_ONLINE_USERS">>,
            <<"list">> => Users
        }
    ),
    io:format("Message ~p~n", [Message]),
    {[{text, Message}], State}.



% Handle a request for logout
handle_logout(State) ->
    io:format("logout request received~n"),
    gen_server:cast(?CHATROOM_SERVER, {logout, {self()}}),
    {ok, State}.



% Handle a new message sent in the chatroom
handle_chat_message(Map, State) ->
    io:format("message received~n"),
    {ok, Username} = maps:find(<<"username">>, Map),
    {ok, Text} = maps:find(<<"text">>, Map),
    {ok, Course} = maps:find(<<"course">>, Map),
    gen_server:cast(
        ?CHATROOM_SERVER, 
        {send_message, {self(), binary_to_list(Username), Course, binary_to_list(Text)}}
    ),
    {ok, State}.


% Cowboy will call websocket_info/2 whenever an Erlang message arrives 
% (=> from another Erlang process).
websocket_info({send_message, Msg}, State) ->
    io:format("chatroom_websocket:websocket_info(send_message) => Send message ~p~n", [Msg]),
    {[{text, Msg}], State};

websocket_info(Info, State) ->
    io:format("chatroom_websocket:websocket_info(?) => Received info ~p~n", [Info]),
    {ok, State}.


% Cowboy will call terminate/3 with the reason for the termination of the connection. 
terminate(_Reason, _Req, _State) ->
    % Logout user from chatroom
    io:format("chatroom_websocket:terminate~n"),
    gen_server:cast(?CHATROOM_SERVER, {logout, self()}),
    ok.
