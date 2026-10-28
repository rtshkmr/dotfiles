#!/usr/bin/env bash
# Recreate the cluster of the current worktree.
# Usage: pg-reset.sh [-y]   (-y skips the prompt; use it from Emacs compile buffers)
set -euo pipefail
d="$(dirname "${BASH_SOURCE[0]}")"
source "$d/pgenv.sh"
pg_require_install

if [[ "${1:-}" != "-y" ]]; then
	read -r -p "Delete $PGDATA (worktree $PG_WORKTREE_ID, port $PGPORT)? [y/N] " ans || ans=n
	[[ "$ans" == [yY]* ]] || {
		echo "Aborted."
		exit 1
	}
fi

"$d/pg-stop.sh" || true
rm -rf "${PGDATA:?}"
"$d/pg-init.sh"
"$d/pg-start.sh"
