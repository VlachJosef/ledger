#!/bin/bash

set -e #Exit immediately if a command exits with a non-zero status.

function help {
    echo "Usage: $0 [client_id] [node_id] [secret_key]"
    echo "Options:"
    echo "  client_id  - id of the client node"
    echo "  node_id    - id of the node which to connect"
    echo "  secret_key - secret key"
}

case "$#" in
    3)  echo "Launching client with id $1. Connecting to node $2. Using key $3."
    	CLIENT_ID=$1
    	NODE_ID=$2
	SECRET_KEY=$3
	SOCKET_FILE="./sockets/${CLIENT_ID}.sock"

	if [ -e "$SOCKET_FILE" ]; then
	    echo "Removing $SOCKET_FILE"
	    rm $SOCKET_FILE
	fi

	stack build
	./.stack-work/install/x86_64-osx/lts-10.3/8.2.2/bin/crypto-ledger-client -c $CLIENT_ID -n $NODE_ID -k keys/$SECRET_KEY
	;;
    *)
	help
	;;
esac
