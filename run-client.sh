#!/bin/bash

set -e #Exit immediately if a command exits with a non-zero status.

function help {
    echo "Usage: $0 [client_id] [node_id]"
    echo "Options:"
    echo "  client_id - id of the client node"
    echo "  node_id   - id of the node which to connect"
}

case "$#" in
    1)
	echo "Launching client NodeId $1"

	CLIENT_ID=$1
	SOCKET_FILE="./sockets/${CLIENT_ID}.sock"

	if [ -e "$SOCKET_FILE" ]; then
	    echo "Removing $SOCKET_FILE"
	    rm $SOCKET_FILE
	fi

	stack build
	./.stack-work/install/x86_64-osx/lts-10.3/8.2.2/bin/crypto-client -c $CLIENT_ID -k keys/8bded39f1092ecaec9c7d63cb6610c450e748b355abc82593d7a01c4dc8ef0f0
	;;
    2)  echo "Launching client with id $1. Connecting to node $2."
    	CLIENT_ID=$1
    	NODE_ID=$2
	SOCKET_FILE="./sockets/${CLIENT_ID}.sock"

	if [ -e "$SOCKET_FILE" ]; then
	    echo "Removing $SOCKET_FILE"
	    rm $SOCKET_FILE
	fi

	stack build
	./.stack-work/install/x86_64-osx/lts-10.3/8.2.2/bin/crypto-client -c $CLIENT_ID -n $NODE_ID -k keys/8bded39f1092ecaec9c7d63cb6610c450e748b355abc82593d7a01c4dc8ef0f0
	;;
    *)
	help
	;;
esac