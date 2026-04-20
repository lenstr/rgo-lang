#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."
mkdir -p .factory/runtime
nix develop -c true >/dev/null
