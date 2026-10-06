#!/usr/bin/env bash
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/pgenv.sh"

if [[ -f "$PGDATA/PG_VERSION" ]]; then
	echo "Cluster already exists at $PGDATA (use pg-reset.sh to recreate)"
	exit 1
fi

initdb -D "$PGDATA" --no-sync -E UTF8 --no-locale

cat >>"$PGDATA/postgresql.conf" <<EOF

# --- dev overrides ---
port = $PGPORT
log_line_prefix = '%m [%p] %q%u@%d '     # %p puts the backend PID in every log line
fsync = off                              # dev only, never for real data
#debug_print_parse = on
#debug_print_plan = on
EOF
