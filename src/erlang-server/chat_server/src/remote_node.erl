-module(remote_node).

-export([start/0, stop/0]).

start() ->
	application:start(chat_server).

stop() ->
	application:stop(chat_server).
