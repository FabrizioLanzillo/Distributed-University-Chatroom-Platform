-module(chatroom_server).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_cast/2, handle_call/3]).


start_link() ->
	io:format("chatroom_server:start_link()~n"),
	{ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
	io:format("chatroom_server pid is ~p~n", [Pid]),
	{ok, Pid}.


init(_) ->
	io:format("chatroom_server:init()~n"),
	Dispatch = cowboy_router:compile([
		{'_', [{"/", chatroom_websocket, []}]}
	]),
	{ok, _} = cowboy:start_clear(chatroom_listener,
		[{port, 8080}],
		#{env => #{dispatch => Dispatch}}
	),
	{ok, []}.


handle_cast(_, State) ->
    io:format("Hi, you just invoked cast~n"),
	{noreply, State}.

handle_call(_, _From, State) ->
    io:format("Hi, you just invoked call~n"),
	{reply, hello, State}.
