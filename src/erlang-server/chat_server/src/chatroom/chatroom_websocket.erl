-module(chatroom_websocket).

-export([init/2, websocket_handle/2, websocket_info/2, terminate/3]).


% Cowboy will call init/2 whenever a request is received,
% in order to establish a websocket connection.
init(Req, _State) ->
    % Switch to cowboy_websocket module
    {cowboy_websocket, Req, none}.


% Cowboy will call websocket_handle/2 whenever a text, binary, ping or pong frame arrives from the client.
websocket_handle(Frame = {text, Message}, State) ->
    io:format("[chatroom_websocket] websocket_handle => Frame: ~p, State: ~p~n", [Frame, State]),
    io:format("[chatroom_websocket] websocket_handle => Received ~p~n", [Frame]),
    
    DecodedMessage = jsone:try_decode(Message),

    Response = case element(1, DecodedMessage) of
        ok ->
            Json = element(2, DecodedMessage),
            handle_websocket_frame(Json, State);
        error ->
            io:format("[chatroom_websocket] websocket_handle => jsone:try_decode: error: ~p~n",[element(2, DecodedMessage)]),
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
        <<"GET_ONLINE_USERS">> ->
            handle_get_online_users(State);
        <<"MESSAGE">> ->
            handle_chat_message(Map, State)
    end,
    Response.



% Handle a login request
handle_login(Map, _State) ->
    {ok, Course} = maps:find(<<"course">>, Map),
    {ok, Username} = maps:find(<<"username">>, Map),
    io:format(
        "[chatroom_websocket] handle_login => login request received for course ~p by user ~p~n",
        [Course, Username]
    ),
    chatroom_server:login(self(), Username, Course),
    {ok, {Course, Username}}. % init state with course id and username



% Handle a request for updating online users
handle_get_online_users(State = {Course, _}) ->
    io:format("[chatroom_websocket] handle_get_online_users => get_online_users request received~n"),
    Message = chatroom_server:get_online_students(Course),
    io:format("[chatroom_websocket] handle_get_online_users => Message ~p~n", [Message]),
    {[{text, Message}], State}.



% Handle a new message sent in the chatroom
handle_chat_message(Map, State = {Course, Username}) ->
    io:format("[chatroom_websocket] handle_chat_message => message received from Pid: ~p in the course: ~p ~n", [self(), Course]),
    {ok, Text} = maps:find(<<"text">>, Map),
    chatroom_server:send_message(self(),
                                binary_to_list(Username), 
                                Course, 
                                binary_to_list(Text)),
    {ok, State}.


% Cowboy will call websocket_info/2 whenever an Erlang message arrives 
% (=> from another Erlang process).
websocket_info({send_message, Msg}, State) ->
    io:format("[chatroom_websocket] websocket_info({send_message, Msg}, State) => Send message ~p~n", [Msg]),
    {[{text, Msg}], State};

websocket_info(Info, State) ->
    io:format("chatroom_websocket:websocket_info(Info, State) => Received info ~p~n", [Info]),
    {ok, State}.


% Cowboy will call terminate/3 with the reason for the termination of the connection. 
terminate(_Reason, _Req, _State = {Course, Username}) ->
    % Logout user from chatroom
    io:format("[chatroom_websocket] terminate => logout request received from Pid: ~p in the course: ~p ~n", [self(), Course]),
    chatroom_server:logout(self(), Username, Course),
    ok.
