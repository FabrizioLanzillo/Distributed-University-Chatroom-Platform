%%%-------------------------------------------------------------------
%% Module that instantiate a chatroom server using Cowboy library.
%%%-------------------------------------------------------------------
-module(chatroom_server).
-behaviour(gen_server).
-include("chat.hrl").

-export([start_link/0, init/1, handle_cast/2, handle_call/3, terminate/2]).



start_link() ->
	io:format("chatroom_server:start_link()~n"),
	% Start a gen_server using the current module as a callback module
	{ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
	io:format("chatroom_server pid is ~p~n", [Pid]),
	{ok, Pid}.



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



handle_call(Req, From, State) ->
    io:format("chatroom_server: invoked call from ~p with request message ~p~n", [From, Req]),
	{reply, hello, State}.



handle_cast({logout, Pid}, State) ->
	io:format("chatroom_server: User ~p is executing logout~n", [Pid]),
	gen_server:cast(?COURSE_MANAGER, {logout, Pid}),
	{noreply, State};

handle_cast({send_message, {PidSender, SenderName, CourseId, Text}}, State) ->
	case gen_server:call(?COURSE_MANAGER, {get_online_users, CourseId}) of
		List when is_list(List), List /= [] ->
			send_message_in_chatroom(List, PidSender, SenderName, Text);
		_ ->
			PidSender ! {send_message, PidSender, "Error: course does not exist~n"}
	end,
	{noreply, State};

handle_cast(Request, State) ->
    io:format("chatroom_server: Invoked cast with the following request message: ~p~n", [Request]),
	{noreply, State}.



terminate(_Reason, _State) ->
	% Stop cowboy listener
	cowboy:stop_listener(chatroom_listener),
    ok.



send_message_in_chatroom([], _, _, _) ->
	ok;

send_message_in_chatroom([PidReceiver | T], PidSender, SenderName, Text) when PidReceiver /= PidSender ->
	Message = jsone:encode(
		#{
			<<"opcode">> => <<"MESSAGE">>,
			<<"sender">> => <<SenderName>>,
			<<"text">> => <<Text>>
		}
	),
	PidReceiver ! {send_message, Message},
	send_message_in_chatroom(T, PidSender, SenderName, Text);

send_message_in_chatroom([_H | T], PidSender, SenderName, Text) ->
	send_message_in_chatroom(T, PidSender, SenderName, Text).
