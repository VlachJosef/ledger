#!/bin/bash

set -e #Exit immediately if a command exits with a non-zero status.

function help {
    echo "Usage: $0 [node_count] [transaction_id"
    echo "Options:"
    echo "  node_count - number of nodes in cluster"
    echo "  transaction_id - transaction id to query"
}

function clean {
    for i in "${NODE_PIDS[@]}"; do
	kill $i
    done
}

# http://redsymbol.net/articles/bash-exit-traps/
trap clean EXIT

NODE_COUNT=$1
TRANSACTION_ID=$2

case "$#" in
    2)
       for ((i=0; i<$NODE_COUNT; i++));
       do
	   SOCKET_FILE="./sockets/$(expr $i + 100).sock"
	   echo "QUERY $TRANSACTION_ID" | nc -U $SOCKET_FILE &
	   PID=$!
	   NODE_PIDS[$PID]=$PID
       done
       sleep 0.1
       ;;
    *)
	help
	;;
esac
