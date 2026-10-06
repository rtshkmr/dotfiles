#!/usr/bin/env bash
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/pgenv.sh"
cd "$PG_SRC"

[[ "${1:-}" == "--wipe" ]] && rm -rf "$PG_BUILD"

opts=(--prefix="$PG_PREFIX" --buildtype=debug -Dcassert=true)
if [[ -d "$PG_BUILD" ]]; then
	meson setup "$PG_BUILD" --reconfigure "${opts[@]}"
else
	meson setup "$PG_BUILD" "${opts[@]}"
fi
