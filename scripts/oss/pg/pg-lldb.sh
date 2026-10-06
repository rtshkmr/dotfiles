#!/usr/bin/env bash
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/pgenv.sh"
pid="${1:-}"
if [[ -z "$pid" ]]; then # default: the newest *other* client backend
	pid=$(psql -X -Atc "select pid from pg_stat_activity
        where backend_type='client backend' and pid <> pg_backend_pid()
        order by backend_start desc limit 1")
fi
[[ -n "$pid" ]] || {
	echo "No client backend found. Open psql first."
	exit 1
}
exec lldb -p "$pid"
