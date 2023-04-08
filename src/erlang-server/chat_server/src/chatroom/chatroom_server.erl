%%%-------------------------------------------------------------------
%% Module that instantiate a chatroom server using Cowboy library.
%%%-------------------------------------------------------------------
-module(chatroom_server).
-behavior(gen_server).
-include("chat.hrl").

-export([start_link/0, init/1, handle_cast/2, handle_call/3, terminate/2]).



start_link() ->
	io:format("chatroom_server:start_link()~n"),
	% Start a gen_server using the current module as a callback module
	{ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
	io:format("chatroom_server pid is ~p~n", [Pid]),
	{ok, Pid}.



% Initialize gen_server
init(_) ->
	io:format("chatroom_server:init()~n"),
	% Compile the route for the websocket handler
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", chatroom_websocket, []}
		]}
	]),
	% Start listening for connections over a clear TCP channel 
	{ok, Pid} = cowboy:start_clear(chatroom_listener,
		[{port, 8080}],
		#{env => #{dispatch => Dispatch}}
	),
	io:format("Cowboy server pid: ~p~n", [Pid]),
	{ok, []}.



% Handle request for updating online users
handle_call({update_online_users, {Pid, CourseId}}, _From, State) when is_pid(Pid) ->
	io:format("chatroom_server: get list of online users for course ~p~n", [CourseId]),
	Users = gen_server:call(?COURSE_MANAGER, {get_online_users, CourseId}), % TODO write ad hoc function
	{reply, Users, State};

% Handle any call
handle_call(Req, From, State) ->
    io:format("chatroom_server: invoked call from ~p with request message ~p~n", [From, Req]),
	{reply, hello, State}.




% Handle cast for login
handle_cast({login, {Pid, Course, Username}}, State) ->
	io:format("chatroom_server: User ~p (~p) is executing login~n", [Pid, Username]),
  gen_server:call(?COURSE_MANAGER, {join_course, Course, Pid}),
  {noreply, State};


% Handle cast for logout
handle_cast({logout, Pid}, State) ->
	io:format("chatroom_server: User ~p is executing logout~n", [Pid]),
	% Forward the logout request to the course manager, so that the user can
	% be unsubscribe from the course chatroom
	gen_server:cast(?COURSE_MANAGER, {logout, Pid}),
	{noreply, State};


% Handle cast for a "send message" request
handle_cast({send_message, {PidSender, SenderName, CourseId, Text}}, State) ->
	io:format(
		"chatroom_server: User ~p is sending message in chatroom of course ~p~n", 
		[SenderName, CourseId]
	),

	% Get list of currently online users and forward the message to all of them
	case gen_server:call(?COURSE_MANAGER, {get_online_users, CourseId}) of

		List when is_list(List), List /= [] ->
			io:format("chatroom_server: send message to ~p~n", [List]),
			% Prepare the message as a JSON document
			Message = jsone:encode(
				#{
					<<"opcode">> => <<"MESSAGE">>,
					<<"sender">> => list_to_binary(SenderName),
					<<"text">> => list_to_binary(Text)
				}
			),
			% Send the message inside the chatroom
			send_message_in_chatroom(List, PidSender, Message);

		_ ->
			io:format("chatroom_server: error the course does not exist"),
			% The list was empty or the call returned a wrong result, 
			% so the course does not exist
			PidSender ! {send_message, PidSender, "Error: course does not exist~n"}
	end,

	{noreply, State};


% Handle badly-formatted cast requests
handle_cast(Request, State) ->
    io:format("chatroom_server: invoked cast with the following request message: ~p~n", [Request]),
	{noreply, State}.




terminate(_Reason, _State) ->
	io:format("chatroom_server: terminate"),
	% Stop cowboy listener
	cowboy:stop_listener(chatroom_listener),
    ok.



%% send_message_in_chatroom/3: send a message in a chatroom

send_message_in_chatroom([], _PidSender, _Message) ->
	% No more users to send the message => stop recursion
	ok;

send_message_in_chatroom([{PidReceiver, _} | T], PidSender, Message) when PidReceiver /= PidSender ->
	% Send the message to all users except from the sender
	io:format("chatroom_server:send_message_in_chatroom => Sending message ~p to ~p~n", [Message, PidSender]),
	PidReceiver ! {send_message, Message},
	send_message_in_chatroom(T, PidSender, Message);

send_message_in_chatroom([_H | T], PidSender, Message) ->
	% The current head of the PID list is the sender process, so
	% do not send a message but keep going with the recursion
	send_message_in_chatroom(T, PidSender, Message).
