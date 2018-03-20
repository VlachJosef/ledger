#!/bin/bash

set -e #Exit immediately if a command exits with a non-zero status.

stack build
stack exec crypto-launcher
