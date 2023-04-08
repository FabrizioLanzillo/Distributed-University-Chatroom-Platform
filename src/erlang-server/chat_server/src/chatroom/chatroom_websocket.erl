-module(chatroom_websocket).

-export([init/2, websocket_handle/2, websocket_info/2, terminate/3]).
-include("chat.hrl").



% Cowboy will call init/2 whenever a request is received,
% in order to establish a websocket connection.
init(Req, Opts) ->
    % Switch to cowboy_websocket module
    {cowboy_websocket, Req, Opts}.


% TODO
% Cowboy will call websocket_handle/2 whenever a text, binary, ping or pong frame arrives from the client.
websocket_handle(Frame = {text, _Message}, State) ->
    io:format("Received ~p~n", [Frame]),
    {[Frame], State};

websocket_handle(_Frame, State) ->
    {ok, State}.


% TODO
% Cowboy will call websocket_info/2 whenever an Erlang message arrives 
% (=> from another Erlang process).
websocket_info({send_message_chatroom, _ServerPid, Msg}, State) ->
    {[{text, Msg}], State};

websocket_info(_Info, State) ->
    {ok, State}.


% TODO
% Cowboy will call terminate/3 with the reason for the termination of the connection. 
terminate(_Reason, _Req, _State) ->
    % Logout user from chatroom
    gen_server:cast(?CHATROOM_SERVER, {logout, self()}),
    ok.
