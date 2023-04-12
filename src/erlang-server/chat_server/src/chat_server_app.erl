-module(chat_server_app).
-behavior(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % Start supervisor
    mnesia_manager:mnesia_start(), % TODO remove
    chat_server_sup:start_link().

stop(_State) ->
    ok.
