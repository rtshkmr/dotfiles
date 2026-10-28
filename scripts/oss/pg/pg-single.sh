#!/usr/bin/env bash

# Run a single-user backend under the debugger (lldb on macOS, gdb on Linux).
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/pgenv.sh"
pg_check_debugger

pg_ctl -D "$PGDATA" status >/dev/null 2>&1 && {
	echo "Stop the server first."
	exit 1
}
exe="$PG_PREFIX/bin/postgres"
[[ -x "$exe" ]] || {
	echo "$exe not found. Run pg-setup.sh and pg-build.sh first."
	exit 1
}
pg_debug_run "$exe" --single -D "$PGDATA" postgres
