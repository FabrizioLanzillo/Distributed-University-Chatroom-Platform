%%%-------------------------------------------------------------------
%% Module that instantiate a chatroom server using Cowboy library.
%%%-------------------------------------------------------------------
-module(chatroom_server).
-include("chat.hrl").

-export([get_online_students/1, login/2, logout/2, send_message/4, send_message_in_chatroom/3]).



% Get list of currently online users
get_online_students(Course) when is_integer(Course) ->
	io:format("[chatroom_server] get_online_students => course ~p~n", [Course]),
	?MNESIA_MANAGER:get_online_students(Course).



% Execute login for user
login(Pid, Course) when is_pid(Pid), is_integer(Course) ->
	io:format("[chatroom_server] login => pid ~p, course ~p~n", [Pid, Course]),
	?MNESIA_MANAGER:join_course(Pid, Course).



% Execute logout for user
logout(Pid, Course) when is_pid(Pid), is_integer(Course)->
	io:format("[chatroom_server] logout => pid ~p, course ~p~n", [Pid, Course]),
	% Remove the websocket PID from DB list of users inside the chatroom
	?MNESIA_MANAGER:logout(Pid, Course),
	ok.



% Send a message in the chatroom
send_message(PidSender, SenderName, Course, Text) 
		when is_pid(PidSender), is_list(SenderName), is_integer(Course), is_list(Text) ->
	io:format(
		"[chatroom_server] send_message => pid ~p, username ~p, course ~p, text ~p~n", 
		[PidSender, SenderName, Course, Text]
	),
	% Get list of currently online users and forward the message to all of them
	case ?MNESIA_MANAGER:get_online_students(Course) of

		List when is_list(List), List /= [] ->
			% Prepare the message as a JSON document
			Message = jsone:encode(
				#{
					<<"opcode">> => <<"MESSAGE">>,
					<<"sender">> => list_to_binary(SenderName),
					<<"text">> => list_to_binary(Text)
				}
			),
			io:format("[chatroom_server] send_message => send message ~p to ~p~n", [Message, List]),
			% Send the message inside the chatroom
			send_message_in_chatroom(List, PidSender, Message);

		_ ->
			io:format("[chatroom_server] send_message => error the course does not exist"),
			% The list was empty or the call returned a wrong result, 
			% so the course does not exist
			PidSender ! {send_message, PidSender, "Error: course does not exist~n"}
	end,
	ok.



%% send_message_in_chatroom/3: send a message in a chatroom
send_message_in_chatroom([], _PidSender, _Message) when is_pid(_PidSender) ->
	% No more users to send the message => stop recursion
	ok;

send_message_in_chatroom([PidReceiver | T], PidSender, Message) 
		when is_pid(PidReceiver), is_pid(PidSender), PidReceiver /= PidSender ->
	% Send the message to all users except from the sender
	io:format("[chatroom_server] send_message_in_chatroom => sending message to ~p~n", [PidReceiver]),
	PidReceiver ! {send_message, Message},
	send_message_in_chatroom(T, PidSender, Message);

send_message_in_chatroom([_H | T], PidSender, Message) when is_pid(_H), is_pid(PidSender)->
	% The current head of the PID list is the sender process, so
	% do not send a message but keep going with the recursion
	send_message_in_chatroom(T, PidSender, Message).
