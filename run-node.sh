#!/bin/bash

set -e #Exit immediately if a command exits with a non-zero status.

function help {
    echo "Usage: $0 [node_id] [node_count]"
    echo "Options:"
    echo "  node_id - id of the node"
    echo "  node_count - number of nodes in cluster"
}

case "$#" in
    2)
	echo "Launching NodeId $1 of cluster size $2"

	NODE_ID=$1
	NODE_COUNT=$2
	SOCKET_FILE="./sockets/${NODE_ID}.sock"

	if [ -e "$SOCKET_FILE" ]; then
	    echo "Removing $SOCKET_FILE"
	    rm $SOCKET_FILE
	fi

	stack build
	./.stack-work/install/x86_64-osx/lts-10.3/8.2.2/bin/crypto-ledger-node $NODE_ID $NODE_COUNT sockets 0 0 0 Distribution.keys
	;;
    *)
	help
	;;
esac
