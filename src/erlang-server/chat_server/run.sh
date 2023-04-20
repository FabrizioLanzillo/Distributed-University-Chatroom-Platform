#!/bin/sh

# Remove Mnesia database
rm -r Mnesia*

# Start Erlang node. Requires the name of the node as argument of the script 
# 		=> ./run.sh server@<ip-address>
rebar3 shell --name server@127.0.0.1 --setcookie chat_application
