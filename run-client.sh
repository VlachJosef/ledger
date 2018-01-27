#!/bin/bash

set -e #Exit immediately if a command exits with a non-zero status.

function help {
    echo "Usage: $0 [node_id]"
    echo "Options:"
    echo "  node_id - id of the client node"
}

case "$#" in
    1)
	echo "Launching client NodeId $@"

	NODE_ID=$1
	SOCKET_FILE="./sockets/${NODE_ID}.sock"

	if [ -e "$SOCKET_FILE" ]; then
	    echo "Removing $SOCKET_FILE"
	    rm $SOCKET_FILE
	fi

	stack build
	./.stack-work/install/x86_64-osx/lts-10.3/8.2.2/bin/crypto-client -k keys/8bded39f1092ecaec9c7d63cb6610c450e748b355abc82593d7a01c4dc8ef0f0
	;;
    *)
	help
	;;
esac
