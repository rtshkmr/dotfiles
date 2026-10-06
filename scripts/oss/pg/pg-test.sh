#!/usr/bin/env bash
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/pgenv.sh"
# Default: setup + core regression. Pass your own args to override,
# e.g. ./pg-test.sh --suite setup --suite recovery
args=("$@")
[[ ${#args[@]} -eq 0 ]] && args=(--suite setup --suite regress)
meson test -C "$PG_BUILD" "${args[@]}"
