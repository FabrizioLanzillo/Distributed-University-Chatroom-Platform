%%%-------------------------------------------------------------------
%% @doc load_balancer public API
%% @end
%%%-------------------------------------------------------------------

-module(load_balancer_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % Connect to cluster nodes
    {ok, Nodes} = application:get_env(nodes),
    connect_nodes(Nodes),
    % Configure mnesia
    config_mnesia([self(), Nodes]),
    % Start nodes
    start_nodes(Nodes).



stop(_State) ->
    mnesia:stop(),
    ok.



connect_nodes([]) ->
    ok;

connect_nodes([H | T]) when is_atom(H), is_list(T) ->
    true = net_kernel:connect_node(H),
    connect_nodes(T).



start_nodes([]) ->
    ok;

start_nodes([H | T]) ->
    spawn(H, fun()-> application:start(chat_server) end),
    start_nodes(T).


%%% MNESIA

-record(online_students, {course_name, student_pid}).

config_mnesia(Nodes) ->
    % Create mnesia schema if doesn't exists
	Result1 = mnesia:create_schema(Nodes),
    io:format("mnesia:create_schema(~p) => ~p~n", [Nodes, Result1]),
    % Start mnesia application
	mnesia:start(),
	io:format("mnesia:start()~n"),
    % Create table
    Result2 = mnesia:create_table(
        online_students,
        [
            {attributes, record_info(fields, online_students)},
            {type, bag}, {ram_copies, Nodes}
        ]),
	io:format("mnesia:create_table => ~p~n", Result2),
	mnesia:info(),
    ok.
