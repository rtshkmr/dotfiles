#!/usr/bin/env bash
#
# Attach lldb (macOS) or gdb (Linux) to a running backend of this worktree's server.
# Usage: pg-attach.sh [pid]   (default: the newest other client backend)
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/pgenv.sh"
pg_require_install
pg_check_debugger

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
pg_warn_ptrace
pg_debug_attach "$pid"
