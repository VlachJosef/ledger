# crypto-ledger

Quick guide (assuming all commands are run from same folder as this `README.md`):

1. Run `stack build`
2. Run `./.stack-work/install/x86_64-osx/lts-10.3/8.2.2/bin/crypto-ledger-client` to create random secret keys. They will be placed `keys` folder. Run once for every client you want to launch.
3. Run node by running script `./run-node.sh <node_id> <number_of_nodes>`
   For `> ./run-node.sh 0 1`
       `> ./run-node.sh 1 1` will launch two nodes with `node_id`s 0 and 1.
4. Run client by running script `./run-client.sh <client_id> <node_id> <secret_key>` where `<secret_key>` is name of file containing secret key in `keys` folder.
   For example `./run-client.sh 100 0 6517380882af1dad2a46bd32166c485a28ca8cf098f383c96f89d53f957db7c9` will launch client which will connect to node 0
5. Connect to the client by `nc -U ./sockets/<socket_name>`
   For example `nc -U ./sockets/100.sock` will connet to the client with `client_id` 100.
6. Once connected to client you can issue these commands:
     `> status`                    - raw insight into state of connected node (output needs lots of polishing)
     `> BALANCE <address>`         - example: `> BALANCE BQPdvHitAnbz69jqDJB2PyH17MmbRgjQU13zdagpAUP5` returns balance for `<address>`
     `> QUERY <transactionId>`     - example: `> QUERY GCcJmkjWcvZwHB5DTPY97omdSxTbM32kiUuTvn4Zmvfw` returns `true` / `false`
     `> SUBMIT <address> <amount>` - example: `> SUBMIT BQPdvHitAnbz69jqDJB2PyH17MmbRgjQU13zdagpAUP5 100` returns `<transactionId>`



For developing run:
> stack ghci --test crypto-ledger:lib crypto-ledger:crypto-ledger-test
