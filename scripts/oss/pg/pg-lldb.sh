#!/usr/bin/env bash
# Kept for muscle memory; pg-attach.sh does the work.
export PG_DEBUGGER=lldb
exec "$(dirname "${BASH_SOURCE[0]}")/pg-attach.sh" "$@"
