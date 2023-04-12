-module(cowboy_listener).
-behaviour(gen_server).


-export([start_link/0, init/1, handle_call/3, handle_cast/2]).



start_link() ->
	io:format("cowboy_listener:start_link"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init(_) ->
	% Read endpoint and port from configuration file
	{ok, Url} = application:get_env(websocket_endpoint),
	{ok, Port} = application:get_env(websocket_port),
	io:format("[cowboy_listener] init => Start listener on endpoint ~p and port ~p~n", [Url, Port]),
	% Compile the route for the websocket handler
	Dispatch = cowboy_router:compile([
		{'_', [
			{Url, chatroom_websocket, []}
		]}
	]),
	% Start listening for connections over a clear TCP channel 
	{ok, Pid} = cowboy:start_clear(chatroom_listener,
		[{port, Port}],
		#{env => #{dispatch => Dispatch}}
	),
	io:format("Cowboy is listening at process with pid ~p~n", [Pid]),
	{ok, []}.


handle_call(Req, _, State) ->
	{reply, Req, State}.


handle_cast(_, State) ->
	{noreply, State}.
