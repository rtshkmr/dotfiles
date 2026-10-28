#!/usr/bin/env bash
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/pgenv.sh"
pg_require_install
# -p makes the environment (PGPORT) authoritative even if postgresql.conf is stale.
pg_ctl -D "$PGDATA" -l "$PG_PREFIX/server.log" -o "-p $PGPORT" -w start
