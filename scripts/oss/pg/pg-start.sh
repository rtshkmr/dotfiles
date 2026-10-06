#!/usr/bin/env bash
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/pgenv.sh"
pg_ctl -D "$PGDATA" -l "$PG_PREFIX/server.log" -w start
