#!/bin/bash

SCROLLBACK_LINES=10000

set -ex

# load environment
source $HOME/erlbot.env
for KEY in $(cat $HOME/erlbot.env | cut -d'=' -f1)
do
    export $KEY
done

# kill any running erlbot sessions
./bin/erlbot stop || true
screen -S erlbot -X quit || true

# start erlbot in a detached screen
# add -noshell to get rid of erlang shell
screen -S erlbot -L -h "$SCROLLBACK_LINES" -d -m ./bin/erlbot foreground


