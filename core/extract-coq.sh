#!/bin/bash

set -eou pipefail

SCRIPTPATH="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

mkdir -p "$SCRIPTPATH/ml"
mkdir -p "$SCRIPTPATH/hs"
coqtop -compile "$SCRIPTPATH/hazel_core.v"
