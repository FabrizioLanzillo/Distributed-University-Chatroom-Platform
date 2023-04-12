-module(chatroom_websocket).

-export([init/2, websocket_handle/2, websocket_info/2, terminate/3]).
-include("chat.hrl").


% Cowboy will call init/2 whenever a request is received,
% in order to establish a websocket connection.
init(Req, _State) ->
    % Switch to cowboy_websocket module
    {cowboy_websocket, Req, none}.


% Cowboy will call websocket_handle/2 whenever a text, binary, ping or pong frame arrives from the client.
websocket_handle(Frame = {text, _Message}, State) ->
    io:format("[chatroom_websocket] websocket_handle => Frame: ~p, State: ~p~n", [Frame, State]),
    io:format("[chatroom_websocket] websocket_handle => Received ~p~n", [Frame]),
    
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
    io:format("[chatroom_websocket] handle_websocket_frame => Map is ~p~n", [Map]),
    {ok, Opcode} = maps:find(<<"opcode">>, Map),

    Response = case Opcode of
        <<"LOGIN">> ->
            handle_login(Map, State);
        <<"UPDATE_ONLINE_USERS">> ->
            handle_update_online_users(State);
        <<"LOGOUT">> ->
            handle_logout(State);
        <<"MESSAGE">> ->
            handle_chat_message(Map, State)
    end,
    Response.



% Handle a login request
handle_login(Map, _State) ->
    io:format("[chatroom_websocket] handle_login => login request received~n"),
    {ok, Course} = maps:find(<<"course">>, Map),
    {ok, Username} = maps:find(<<"username">>, Map),
    chatroom_server:login(self(), Course),
    {ok, {Course, Username}}. % init state with course id and username



% Handle a request for updating online users
handle_update_online_users(State = {Course, _}) ->
    io:format("[chatroom_websocket] handle_update_online_users => update_online_users request received~n"),
    Users = chatroom_server:update_online_users(Course),
    Message = jsone:encode(
        #{
            <<"opcode">> => <<"UPDATE_ONLINE_USERS">>,
            <<"list">> => Users
        }
    ),
    io:format("[chatroom_websocket] handle_update_online_users => Message ~p~n", [Message]),
    {[{text, Message}], State}.



% Handle a request for logout
handle_logout(State = {Course, _}) ->
    io:format("[chatroom_websocket] handle_logout => logout request received~n"),
    chatroom_server:logout(self(), Course),
    {ok, State}.



% Handle a new message sent in the chatroom
handle_chat_message(Map, State = {Course, Username}) ->
    io:format("[chatroom_websocket] handle_chat_message => message received~n"),
    {ok, Text} = maps:find(<<"text">>, Map),
    chatroom_server:send_message(self(),
                                binary_to_list(Username), 
                                Course, 
                                binary_to_list(Text)),
    {ok, State}.


% Cowboy will call websocket_info/2 whenever an Erlang message arrives 
% (=> from another Erlang process).
websocket_info({send_message, Msg}, State) ->
    io:format("[chatroom_websocket] handle_chat_message => Send message ~p~n", [Msg]),
    {[{text, Msg}], State};

websocket_info(Info, State) ->
    io:format("chatroom_websocket:websocket_info(?) => Received info ~p~n", [Info]),
    {ok, State}.


% Cowboy will call terminate/3 with the reason for the termination of the connection. 
terminate(_Reason, _Req, _State = {Course, _}) ->
    % Logout user from chatroom
    io:format("[chatroom_websocket] terminate => Pid: ~p, Course: ~p ~n", [self(), Course]),
    chatroom_server:logout(self(), Course),
    ok.
