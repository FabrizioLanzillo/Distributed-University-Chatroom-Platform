-module(master_node_app).
-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
	% Connect to cluster nodes
	{ok, Nodes} = application:get_env(nodes),
	io:format("[master_node_app] start => Nodes ~p~n", [Nodes]),
	connect_nodes(Nodes),
	
	% Configure mnesia
	io:format("[master_node_app] start => config mnesia~n"),
	start_mnesia(Nodes),
	
	% Start nodes
	io:format("[master_node_app] start =>  start_nodes~n"),
	start_nodes(Nodes),
	
	% Print info about mnesia DB
	mnesia:info(),
	
	% Return current Pid and state
	{ok, self(), Nodes}.



stop(Nodes) ->
	io:format("[master_node_app] stop => master_node:stop(~p)~n", [Nodes]),
	% Stop mnesia (in another thread)
	spawn(mnesia, stop, []),
	% Stop remote nodes
	stop_nodes(Nodes),
	ok.


%% Connect to remote nodes
connect_nodes([]) ->
	ok;

connect_nodes([H | T]) when is_atom(H), is_list(T) ->
	io:format("[master_node_app] connect_nodes => Connect node ~p~n", [H]),
	true = net_kernel:connect_node(H),
	connect_nodes(T).


%% Start remote nodes
start_nodes([]) ->
	ok;

start_nodes([Node | T]) ->
	io:format("[master_node_app] start_nodes => ~p~n", [Node]),
	spawn(Node, application, start, [chat_server]), % application:start(chat_server)
	start_nodes(T).



%% Stop remote nodes
stop_nodes([]) ->
	ok;

stop_nodes([Node | T]) ->
	io:format("[master_node_app] stop_nodes => ~p~n", [Node]),
	spawn(Node, application, stop, [chat_server]),
	stop_nodes(T).



%%% MNESIA

-record(online_students, {course, student_pid}).

start_mnesia(Nodes) when is_list(Nodes) ->
	% Create mnesia schema if doesn't exists
	Result1 = mnesia:create_schema([node()] ++ Nodes),
	io:format("[master_node_app] start_mnesia => create_schema(~p) => ~p~n", [Nodes, Result1]),
	
	% Start mnesia application
	mnesia:start(),
	io:format("[master_node_app] start_mnesia => start()~n"),
	
	% Create table
	Result2 = mnesia:create_table(
		online_students,
		[
			{attributes, record_info(fields, online_students)},
			{type, bag},
			{ram_copies, Nodes}
		]),
	io:format("[master_node_app] start_mnesia => create_table result: ~p~n", [Result2]),
	ok.
