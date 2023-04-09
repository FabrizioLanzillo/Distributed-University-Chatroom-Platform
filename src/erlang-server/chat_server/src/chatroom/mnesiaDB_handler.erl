- module(mnesiaDB_handler).


-include("chat.hrl").
-export([mnesia_start/0]).

-record(active_users, {course_name, user_id}).

mnesia_start() ->
  % Create mnesia schema if doesn't exists
  Result = mnesia:create_schema([node(), 'TODO']), %TODO
  mnesia:start(),
  case Result of
    ok->
      % Create table
      mnesia:create_table(courses, [{type, bag}, {ram_copies, [node(), 'TODO']}]); %%TODO
    _ ->
      %Print error
      io:format("~p~n", [Result])
  end,
  mnesia:info(),
  ok.