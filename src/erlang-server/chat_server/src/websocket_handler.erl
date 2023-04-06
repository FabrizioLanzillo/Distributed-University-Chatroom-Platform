-module(websocket_handler).

%% API
-export([init/2, websocket_handle/2, terminate/3, websocket_info/2]).
% -include("chat.hrl").

init(Req, State) ->
	{cowboy_websocket, Req, State}.

