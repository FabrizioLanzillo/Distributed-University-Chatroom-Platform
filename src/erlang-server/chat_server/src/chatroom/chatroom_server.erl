%%%-------------------------------------------------------------------
%% Module that instantiate a chatroom server using Cowboy library.
%%%-------------------------------------------------------------------
-module(chatroom_server).
-behaviour(gen_server).

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



% TODO 
handle_cast(_, State) ->
    io:format("Hi, you just invoked cast~n"),
	{noreply, State}.



% TODO
handle_call(_, _From, State) ->
    io:format("Hi, you just invoked call~n"),
	{reply, hello, State}.



terminate(_Reason, _State) ->
	% Stop cowboy listener
	cowboy:stop_listener(chatroom_listener),
    ok.
