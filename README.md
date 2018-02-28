# crypto-ledger

1. Run `./launcher.sh`
2. Once Launcher is running run commands from below to interact with cluster.

## Launcher commands

`cluster n` - Run n nodes every node will have associated client with its own Secret Key.

`status`    - Print basic Launcher state (running nodes, clients and if some node is stopped or not).

`terminate` - Will stop all running nodes and clients.

`stop n`    - According the task definition at any time only one node can be down. This command will bring down node `n`.

`launch`    - This command is complement of `stop n`, it will launch previously stopped node `n`.

`script <scriptName>` - Will execute commands from the script one after another. Scripts must be placed in `scripts` folder.

Once cluster is running there is option to launch any command on single or all running nodes. To execute client command on all running node prepedn command with `all`:

`all status`                    - raw insight into state of connected node (output needs lots of polishing).

`all submit <address> <amount>` - will submit `<amount>` to the adresss `<address>`, returns `<transactionId>`.

`all balance <address>`         - returns balance for `<address>`.

`all query <transactionId>`     - returns `true` / `false` depending if transaction was processed or not on the node.

To run client command on single client instead of `all` use clientId, hint use autocomplete to see available commands and clientIds.

## Script example

For script example see `scripts/initial`. This script looks similar to this:
```
cluster 3
all status
all submit 97af2031b33efb033c7c377c53dc94a22c6be60839efb23201a9fdda2b8ab785 50
all submit 97af2031b33efb033c7c377c53dc94a22c6be60839efb23201a9fdda2b8ab785 50
all submit 97af2031b33efb033c7c377c53dc94a22c6be60839efb23201a9fdda2b8ab785 50
```

To execute this script from launcher use `script initial`. Hint use autocomplete to see all available scripts.

This script will launch cluster of three nodes, print the status of the nodes after launch and then on every node will execute three transaction to transfer amount of `50` to address `97af2031b33efb033c7c377c53dc94a22c6be60839efb23201a9fdda2b8ab785`.

Note that addresses and secretkeys of clients are generated with constant seed so on every launch the same SecretKeys are generated (if necessary) and same `Distribution.keys` is produced.
