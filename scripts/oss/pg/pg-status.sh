#!/usr/bin/env bash
source "$(dirname "${BASH_SOURCE[0]}")/pgenv.sh"
pg_ctl -D "$PGDATA" status
tail -n 20 "$PG_PREFIX/server.log" 2>/dev/null
