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
    io:format("Nodes ~p~n", [Nodes]),
    connect_nodes(Nodes),
    % Configure mnesia
    io:format("config mnesia~n"),
    config_mnesia([node()] ++ Nodes),
    % Start nodes
    io:format("start_nodes~n"),
    start_nodes(Nodes),
    % Return
    {ok, self()}.



stop(_State) ->
    mnesia:stop(),
    % Kill remote nodes?
    ok.



connect_nodes([]) ->
    ok;

connect_nodes([H | T]) when is_atom(H), is_list(T) ->
    io:format("Connect node ~p~n", [H]),
    true = net_kernel:connect_node(H),
    connect_nodes(T).



start_nodes([]) ->
    ok;

start_nodes([H | T]) ->
    spawn(H, application, start, [chat_server]),
    start_nodes(T).


%%% MNESIA

-record(online_students, {course_name, student_pid}).

config_mnesia(Nodes) when is_list(Nodes) ->
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
	io:format("mnesia:create_table => ~p~n", [Result2]),
	mnesia:info(),
    ok.
