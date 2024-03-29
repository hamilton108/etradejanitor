#!/bin/bash

HOME="/home/rcs/opt/haskell/etradejanitor"

JANITOR_VER="${janitorVersion}"

PG="172.20.1.3"

REDIS="172.20.1.2"

AMQP="172.20.1.4"

NETWORK="pgutil_lofsrud"

/usr/bin/docker run --network $NETWORK --user $(id -u):$(id -g)  -i --rm -v $HOME:$HOME rcs/etradejanitor:$JANITOR_VER $PG $REDIS $AMQP "$@"

exit 0


