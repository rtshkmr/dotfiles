#!/usr/bin/env bash

# Temporarily open Yama's ptrace restriction (Linux) so gdb can attach to a
# running backend, then close it again by restoring the original value.
# This is system-wide, so it works from any directory (no Postgres worktree needed).
#
# From inside gdb, right after attaching:
#   (gdb) shell pg-yama.sh close      (gdb also accepts ! for shell)
#
# Usage: pg-yama.sh [open|close|toggle|status]   (default: toggle)
set -euo pipefail

if [[ "$(uname -s)" != Linux ]]; then
	echo "Yama is Linux-only; nothing to do on $(uname -s)."
	exit 0
fi

f="${PG_YAMA_FILE:-/proc/sys/kernel/yama/ptrace_scope}"
# XDG_RUNTIME_DIR is a tmpfs, so a stale saved value can't survive a reboot.
state="${XDG_RUNTIME_DIR:-/tmp}/pg-yama-orig"

[[ -r "$f" ]] || {
	echo "No $f: Yama isn't active on this kernel, so there is nothing to toggle."
	exit 0
}

cur="$(<"$f")"

set_scope() { # set_scope <value>
	sudo sysctl -q "kernel.yama.ptrace_scope=$1"
	echo "ptrace_scope: $(<"$f")"
}

open_scope() {
	if [[ "$cur" == 0 ]]; then
		echo "ptrace_scope is already 0; nothing to do."
		return 0
	fi
	if [[ "$cur" == 3 ]]; then
		echo "ptrace_scope is 3, which is locked until reboot; cannot change it." >&2
		exit 1
	fi
	echo "$cur" >"$state"
	set_scope 0
	echo "Close it once gdb has attached: pg-yama.sh close"
}

close_scope() {
	local orig=1
	if [[ -r "$state" ]]; then
		orig="$(<"$state")"
	else
		echo "No saved value found; restoring the default (1)."
	fi
	[[ "$orig" =~ ^[0-3]$ ]] || orig=1
	if [[ "$cur" == "$orig" ]]; then
		echo "ptrace_scope is already $orig; nothing to do."
	else
		set_scope "$orig"
	fi
	rm -f "$state"
}

case "${1:-toggle}" in
open) open_scope ;;
close) close_scope ;;
toggle)
	if [[ "$cur" == 0 ]]; then close_scope; else open_scope; fi
	;;
status)
	echo "ptrace_scope: $cur"
	[[ -r "$state" ]] && echo "saved original: $(<"$state")"
	exit 0
	;;
*)
	echo "Usage: ${0##*/} [open|close|toggle|status]" >&2
	exit 2
	;;
esac
