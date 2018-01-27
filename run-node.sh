#!/bin/bash

set -e #Exit immediately if a command exits with a non-zero status.

function help {
    echo "Usage: $0 [node_id]"
    echo "Options:"
    echo "  node_id - id of the node"
}

case "$#" in
    1)
	echo "Launching NodeId $@"

	NODE_ID=$1
	SOCKET_FILE="./sockets/${NODE_ID}.sock"

	if [ -e "$SOCKET_FILE" ]; then
	    echo "Removing $SOCKET_FILE"
	    rm $SOCKET_FILE
	fi

	stack build
	./.stack-work/install/x86_64-osx/lts-10.3/8.2.2/bin/simple-hpack-exe $NODE_ID 1 a 0 0 0
	;;
    *)
	help
	;;
esac
