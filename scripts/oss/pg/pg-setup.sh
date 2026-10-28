#!/usr/bin/env bash
# pg-setup.sh -- configure the meson build of the current worktree (--wipe starts from scratch).
# Compiler: PG_CC from pgenv.sh (clang on macOS, gcc on Linux). Override per run:
#   PG_CC=gcc-14 pg-setup.sh --wipe
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/pgenv.sh"
cd "$PG_SRC"

cc_path="$(command -v "$PG_CC" 2>/dev/null)" || { echo "compiler '$PG_CC' not found" >&2; exit 1; }

# No compiler caches (see README, "Build policy"). Refuse a ccache/sccache
# wrapper or masquerade directory, whether given as PG_CC or found on PATH.
case "$PG_CC $cc_path" in
*ccache*)
	echo "error: PG_CC='$PG_CC' resolves to '$cc_path', which looks like a compiler cache." >&2
	echo "       Use the real compiler (adjust PG_CC or PATH)." >&2
	exit 1
	;;
esac

if [[ "${1:-}" == "--wipe" ]]; then
	rm -rf "${PG_BUILD:?}"
fi

opts=(--prefix="$PG_PREFIX" --buildtype=debug -Dcassert=true)
if [[ -d "$PG_BUILD" ]]; then
	echo "note: reconfiguring the existing build dir; meson fixes the compiler at first configure (use --wipe to change it)"
	meson setup "$PG_BUILD" --reconfigure "${opts[@]}"
else
	CC="$PG_CC" meson setup "$PG_BUILD" "${opts[@]}"
fi

# A reconfigured old build dir keeps its original compiler; make sure that
# wasn't a cache wrapper.
pg_require_no_compiler_cache
echo "compiler: $cc_path (no compiler cache)"
