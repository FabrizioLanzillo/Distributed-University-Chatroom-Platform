-module(course_manager).
-behaviour(gen_server).

%% API
-include("chat.hrl").
-export([start_link/0, init/1, handle_call/3, handle_cast/2]).

start_link() ->
    io:format("course_manager:start_link"),
    gen_server:start_link({local, course_manager}, course_manager, [], []).

init(_) ->
  mnesiaDB_handler:mnesia_start(),
  {ok, []}.

handle_call({join_course, Sport, Pid}, _From, Sports) ->
    Response = mnesiaDB_handler:add_student(Sport, Pid),
    {reply, Response, Sports};

handle_call({get_online_users, Sport}, _From, Sports) ->
  Response = mnesiaDB_handler:get_online_students_for_chatroom(Sport),
  io:format("course_manager:handle_call(get_online_users) => Response ~p~n", [Response]),
  {reply, Response, Sports};

handle_call({logout, Pid}, _From, Sports) ->
  io:format("course_manager:handle_call(logout)"),
  Response = mnesiaDB_handler:logout(Pid),
  io:format("course_manager:handle_call(logout) => Response ~p~n", [Response]),
  {reply, Response, Sports};

handle_call(_Message, _From, State) ->
  {reply, error, State}.

handle_cast({logout, Course, Pid}, _State) ->
  io:format("course_manager:handle_cast(logout)"),
  mnesiaDB_handler:logout(Course, Pid),
  {noreply, _State};

handle_cast(_Message, State) ->
  {noreply, State}.
