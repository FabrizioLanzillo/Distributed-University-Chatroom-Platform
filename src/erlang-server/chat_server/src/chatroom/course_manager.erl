-module(course_manager).
-behaviour(gen_server).

%% API
-include("chat.hrl").
-export([start_link/0, init/1, handle_call/3, handle_cast/2]).

start_link() ->
    gen_server:start_link({local, course_manager}, course_manager, [], []).

init(_) ->
  Sports = ets:new(sports_table, [set]),
  {ok,Sports}.

handle_call({join_course, Sport, Pid}, _From, Sports) ->
    Response = check_and_insert_sport(Sport, Pid, Sports),
    {reply, Response, Sports};

handle_call({get_online_users, Sport}, _From, Sports) ->
  Response = ets:match_object(Sports, {'_', Sport}),
  io:format("course_manager:handle_call(get_online_users) => Response ~p~n", [Response]),
  {reply, Response, Sports};

handle_call({logout, Pid}, _From, Sports) ->
  Response = ets:match_delete(Sports, {Pid, '_'}),
  {reply, Response, Sports};

handle_call(_Message, _From, State) ->
  {reply, error, State}.

handle_cast(_Message, State) ->
  {noreply, State}.

check_and_insert_sport(Sport, Pid, Sports) ->
  case ets:insert_new(Sports, {Pid, Sport}) of
    true -> ok;
    false -> sport_already_inserted
  end.
