# ERLANG

## How to start the Erlang chatroom 

- On all the distributed `chat_server` containers, run `chat_server/run.sh`.
- Run `master_node/run.sh` on the `master_node` container to set up Mnesia and spawn the `chat_server` application processes on the other remote nodes.

## What if a chat_server node crashed, but master_node is still up?

To run the application without the `master_node`, run the `run.sh` script and execute the command `application:start(chat_server).` inside the Erlang shell.
