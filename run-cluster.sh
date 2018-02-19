#!/bin/bash

set -e #Exit immediately if a command exits with a non-zero status.

function help {
    echo "Usage: $0 [node_count]"
    echo "Options:"
    echo "  node_count - number of nodes in cluster"
}

function clean {
    for i in "${NODE_PIDS[@]}"; do
	echo "Killing node with PID $i"
	kill $i
    done
}

# http://redsymbol.net/articles/bash-exit-traps/
trap clean EXIT

NODE_COUNT=$1
FILES=keys/*

ITER=0
for f in $FILES
do
    PRIVATE_KEYS[${ITER}]=$f
    ITER=$(expr $ITER + 1)
done

echo "Total number of private keys: ${#PRIVATE_KEYS[@]}"

if [ "${#PRIVATE_KEYS[@]}" -lt "$NODE_COUNT" ]; then
    echo "Error: Not enough private keys."
    echo "Total number of private keys: ${#PRIVATE_KEYS[@]}"
    echo "Requested cluster size      : $NODE_COUNT"
    exit 1
fi

case "$#" in
    1)
	stack build

	echo "Launching cluster size $1"

	for ((i=0; i<$NODE_COUNT; i++));
	do
	    SOCKET_FILE="./sockets/$i.sock"

	    if [ -e "$SOCKET_FILE" ]; then
		echo "Removing $SOCKET_FILE"
		rm $SOCKET_FILE
	    fi
	done

	for ((i=0; i<$NODE_COUNT; i++));
	do
	    SOCKET_FILE="./sockets/$(expr $i + 100).sock"

	    if [ -e "$SOCKET_FILE" ]; then
		echo "Removing $SOCKET_FILE"
		rm $SOCKET_FILE
	    fi
	done

	for ((i=0; i<$NODE_COUNT; i++));
	do
	    echo "Launching nodeId $i"

	    ./.stack-work/install/x86_64-osx/lts-10.3/8.2.2/bin/crypto-ledger-node $i $NODE_COUNT sockets 0 0 0 Distribution.keys &

	    PID=$!
	    NODE_PIDS[$PID]=$PID
	    echo "Node $i successfully launched. PID is $PID."
	done

	echo "Wait for client launch..."
	sleep 1

	for ((i=0; i<$NODE_COUNT; i++));
	do
	    CLIENT_ID=$(expr $i + 100)
	    echo "Launching clientId $CLIENT_ID to connect to nodeId $i with ${PRIVATE_KEYS[$i]}"
	    ./.stack-work/install/x86_64-osx/lts-10.3/8.2.2/bin/crypto-ledger-client -c $CLIENT_ID -n $i -k ${PRIVATE_KEYS[$i]} &
	    PID=$!
	    NODE_PIDS[$PID]=$PID
	done
	;;
    *)
	help
	exit 0
	;;
esac

echo "Enter anything to kill node cluster."
read
echo "Exiting..."
