#!/bin/sh

# Remove Mnesia database
rm -r Mnesia*

# Start Erlang node. Requires the name of the node as argument of the script 
# 		=> ./run.sh master@<ip-address>
rebar3 shell --name master@127.0.0.1 --setcookie chat_application
