#!/usr/bin/env bash
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/pgenv.sh"
echo "worktree $PG_WORKTREE_ID  port $PGPORT  data $PGDATA"
if [[ -x "$PG_PREFIX/bin/pg_ctl" ]]; then
	pg_ctl -D "$PGDATA" status || true # exits 3 when the server is stopped
else
	echo "not installed yet (run pg-setup.sh && pg-build.sh)"
fi
tail -n 20 "$PG_PREFIX/server.log" 2>/dev/null || true
