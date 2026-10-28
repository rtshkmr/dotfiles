#!/usr/bin/env bash
# pg-worktree-rm.sh -- tear down a linked worktree together with its dev server,
# install, data and log (~/pgsql-dev-worktrees/<id>).
#
# Usage: pg-worktree-rm.sh [-y] [-f] [WORKTREE_DIR]    (default: the current worktree)
#   -y   don't ask for confirmation
#   -f   pass --force to `git worktree remove` (needed if the worktree has
#        untracked or modified files that git would otherwise refuse to discard)
#
# Order matters: stop the server, remove the worktree, and only then delete the
# prefix, so a refusal from git leaves your data intact. The main worktree is
# never removed.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

yes=0
force=0
while getopts ":yf" opt; do
	case $opt in
	y) yes=1 ;;
	f) force=1 ;;
	*)
		echo "usage: ${0##*/} [-y] [-f] [WORKTREE_DIR]" >&2
		exit 2
		;;
	esac
done
shift $((OPTIND - 1))

target="${1:-.}"
[[ -d "$target" ]] || {
	echo "no such directory: $target" >&2
	exit 1
}
cd "$target"
source "$here/pgenv.sh" # derives everything for the *target* worktree

if [[ "$PG_WORKTREE_ID" == main ]]; then
	echo "Refusing to remove the main worktree ($PG_SRC)." >&2
	exit 1
fi
case "$PG_PREFIX" in
"$HOME"/pgsql-dev-worktrees/?*) ;;
*)
	echo "Unexpected prefix '$PG_PREFIX'; refusing to delete it." >&2
	exit 1
	;;
esac

wt="$PG_SRC"
# The first entry of `git worktree list` is the main worktree.
main="$(git worktree list --porcelain | awk '/^worktree /{print substr($0, 10); exit}')"

echo "Worktree:    $wt"
echo "Id / port:   $PG_WORKTREE_ID / $PGPORT"
echo "Will delete: the worktree and $PG_PREFIX (install, data, server.log)"
if ((!yes)); then
	read -r -p "Continue? [y/N] " ans || ans=n
	[[ "$ans" == [yY]* ]] || {
		echo "Aborted."
		exit 1
	}
fi

# 1. Stop this worktree's server, if any.
if [[ -x "$PG_PREFIX/bin/pg_ctl" && -d "$PGDATA" ]]; then
	if pg_ctl -D "$PGDATA" status >/dev/null 2>&1; then
		echo "Stopping server..."
		pg_ctl -D "$PGDATA" -m fast -w stop
	fi
elif [[ -f "$PGDATA/postmaster.pid" ]]; then
	echo "warn: $PGDATA/postmaster.pid exists but there is no pg_ctl to stop it; check for a running postgres." >&2
fi

# 2. Remove the worktree (from the main checkout; we can't remove our own cwd).
cd "$main"
args=(worktree remove)
if ((force)); then args+=(--force); fi
args+=("$wt")
git "${args[@]}"
echo "Removed worktree $wt"

# 3. Remove the install/data/log.
rm -rf "${PG_PREFIX:?}"
echo "Removed $PG_PREFIX"
