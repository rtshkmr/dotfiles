#!/usr/bin/env bash
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/pgenv.sh"
pg_require_no_compiler_cache
ninja -C "$PG_BUILD" install
