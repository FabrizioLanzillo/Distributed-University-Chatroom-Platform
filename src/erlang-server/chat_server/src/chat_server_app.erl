-module(chat_server_app).
-behavior(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    mnesia:start(), % TODO: wait_for_tables?
    % Start supervisor
    chat_server_sup:start_link().

stop(_State) ->
    mnesia:stop(),
    ok.
