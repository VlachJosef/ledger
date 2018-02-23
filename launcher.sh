#!/bin/bash

set -e #Exit immediately if a command exits with a non-zero status.

stack build
./.stack-work/install/x86_64-osx/lts-10.3/8.2.2/bin/crypto-launcher
