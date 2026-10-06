#!/usr/bin/env bash
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/pgenv.sh"
pg_ctl -D "$PGDATA" status >/dev/null 2>&1 && {
	echo "Stop the server first."
	exit 1
}
exec lldb -- postgres --single -D "$PGDATA" postgres
