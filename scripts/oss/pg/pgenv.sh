#!/usr/bin/env bash
#
# Sourced by every pg-*.sh script; not meant to be run directly.
# Needs bash (uses [[ ]]) and stays bash-3.2 compatible for stock macOS.
#
# Everything below is derived from the git worktree you're currently in
# (the cwd), so each worktree gets its own source dir, build dir, install
# prefix, data dir and port. Several can run side by side with no setup.

# --- locate the worktree -----------------------------------------------------
if ! PG_SRC="$(git rev-parse --show-toplevel 2>/dev/null)" || [[ ! -d "$PG_SRC/src/backend" ]]; then
	echo "pgenv.sh: run this from inside a PostgreSQL worktree (cwd: $PWD)" >&2
	return 1
fi
export PG_SRC

# Main worktree: git dir == common dir. Linked worktree: .git/worktrees/<name>.
pg_gitdir="$(cd "$PG_SRC" && cd "$(git rev-parse --git-dir)" && pwd -P)"
pg_common="$(cd "$PG_SRC" && cd "$(git rev-parse --git-common-dir)" && pwd -P)"

if [[ "$pg_gitdir" == "$pg_common" ]]; then
	# Main worktree keeps the old locations and port, so nothing needs migrating.
	PG_WORKTREE_ID=main
	PG_PREFIX="$HOME/pgsql-dev"
	pg_port_offset=0
else
	# Hash the path relative to $HOME so the same layout (e.g. ~/Projects/pg-foo)
	# gets the same id and port on macOS (/Users/...) and Arch (/home/...).
	pg_hash="$(printf '%s' "${PG_SRC#"$HOME"/}" | cksum | awk '{print $1}')"
	PG_WORKTREE_ID="$(basename "$PG_SRC")-$((pg_hash % 100000))"
	PG_PREFIX="$HOME/pgsql-dev-worktrees/$PG_WORKTREE_ID"
	pg_port_offset=$((1 + pg_hash % 999))
fi

export PG_WORKTREE_ID PG_PREFIX
export PG_BUILD="$PG_SRC/build"
export PGDATA="$PG_PREFIX/data"
# Main uses 5433 (avoids a system/Homebrew/pacman instance on 5432); linked
# worktrees get 5434-6432. Override with PG_PORT=... if two ever collide.
export PGPORT="${PG_PORT:-$((5433 + pg_port_offset))}"
export PGDATABASE=postgres
export PATH="$PG_PREFIX/bin:$PATH"
unset pg_gitdir pg_common pg_hash pg_port_offset

# --- OS detection ------------------------------------------------------------
case "$(uname -s)" in
Darwin) PG_OS=macos ;;
Linux) PG_OS=linux ;;
*)
	echo "pgenv.sh: unsupported OS '$(uname -s)'" >&2
	return 1
	;;
esac
export PG_OS

# --- per-OS settings ---------------------------------------------------------
# Overridable from the caller: PG_DEBUGGER (lldb|gdb) and PG_CC, e.g.
#   PG_DEBUGGER=lldb pg-attach.sh        PG_CC=gcc-14 pg-setup.sh --wipe
case "$PG_OS" in
macos)
	# Homebrew's ICU is keg-only, so pkg-config can't see it unless we point at it.
	if command -v brew >/dev/null 2>&1; then
		icu="$(brew --prefix icu4c 2>/dev/null || true)" # may need a versioned name, e.g. icu4c@77
		if [[ -n "$icu" && -d "$icu/lib/pkgconfig" ]]; then
			export PKG_CONFIG_PATH="$icu/lib/pkgconfig${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"
		fi
	fi
	: "${PG_DEBUGGER:=lldb}"
	: "${PG_CC:=clang}"
	;;
linux)
	# Arch: icu, readline, openssl etc. are normal system packages that
	# pkg-config already finds, so no PKG_CONFIG_PATH tweaks are needed.
	: "${PG_DEBUGGER:=gdb}"
	: "${PG_CC:=gcc}"
	;;
esac
export PG_DEBUGGER PG_CC

# Policy: builds never go through a compiler cache (ccache/sccache), so a stale
# or mismatched cache entry can never be the source of a bug. Meson only
# auto-wraps ccache when CC is unset (pg-setup.sh always sets it); this is a
# second line of defence in case a ccache wrapper still ends up in the compiler
# chain, e.g. a masquerade directory early on PATH.
export CCACHE_DISABLE=1

# --- guards and helpers ------------------------------------------------------
# A fresh worktree has no install yet. Without this guard, initdb/pg_ctl/psql
# would silently resolve to a system or Homebrew Postgres further down PATH.
pg_require_install() {
	[[ -x "$PG_PREFIX/bin/postgres" ]] && return 0
	echo "No install for worktree $PG_WORKTREE_ID at $PG_PREFIX." >&2
	echo "Run pg-setup.sh && pg-build.sh first." >&2
	return 1
}

# Fails if this worktree's build dir was configured with a compiler cache
# (its compile_commands.json would then start with "ccache gcc ...").
pg_require_no_compiler_cache() {
	local db="$PG_BUILD/compile_commands.json"
	[[ -f "$db" ]] || return 0
	if grep -qE '"command": *"([^" ]*/)?s?ccache ' "$db"; then
		echo "error: $PG_BUILD was configured with a compiler cache (ccache/sccache)." >&2
		echo "       Run: pg-setup.sh --wipe" >&2
		return 1
	fi
}

pg_check_debugger() {
	command -v "$PG_DEBUGGER" >/dev/null 2>&1 && return 0
	echo "Debugger '$PG_DEBUGGER' not found in PATH." >&2
	case "$PG_OS" in
	macos) echo "Install the Xcode command line tools: xcode-select --install" >&2 ;;
	linux) echo "On Arch: sudo pacman -S $PG_DEBUGGER" >&2 ;;
	esac
	return 1
}

# Attaching to a running backend needs ptrace on a process that isn't our child.
# Linux's Yama module restricts that when ptrace_scope is not 0.
pg_warn_ptrace() {
	local f=/proc/sys/kernel/yama/ptrace_scope scope
	[[ "$PG_OS" == linux && -r "$f" ]] || return 0
	scope="$(<"$f")"
	[[ "$scope" == 0 ]] && return 0
	echo "Note: kernel.yama.ptrace_scope=$scope, so attaching may fail with 'Operation not permitted'." >&2
	echo "      Run 'pg-yama.sh open' first, and 'pg-yama.sh close' once the debugger has attached." >&2
}

pg_debug_attach() { # pg_debug_attach <pid>
	case "$PG_DEBUGGER" in
	lldb) exec lldb -p "$1" ;;
	gdb) exec gdb -p "$1" ;;
	*) echo "Unsupported PG_DEBUGGER '$PG_DEBUGGER' (use lldb or gdb)" >&2; return 1 ;;
	esac
}

pg_debug_run() { # pg_debug_run <program> [args...]
	case "$PG_DEBUGGER" in
	lldb) exec lldb -- "$@" ;;
	gdb) exec gdb --args "$@" ;;
	*) echo "Unsupported PG_DEBUGGER '$PG_DEBUGGER' (use lldb or gdb)" >&2; return 1 ;;
	esac
}

# --- inspect the environment -------------------------------------------------
# pg_env            aligned listing, for humans
# pg_env --export   export NAME=value lines, for: eval "$(pg-env.sh --export)"
pg_env() {
	local vars="PG_WORKTREE_ID PG_SRC PG_BUILD PG_PREFIX PGDATA PGPORT PGDATABASE PG_OS PG_DEBUGGER PG_CC" v
	case "${1:-}" in
	"")
		for v in $vars; do
			printf '%-15s %s\n' "$v" "${!v:-}" # :- keeps set -u callers from aborting
		done
		# Default socket location for a source build; adjust if you ever set
		# unix_socket_directories.
		printf '%-15s %s\n' "socket" "/tmp/.s.PGSQL.$PGPORT"
		;;
	-x | --export)
		for v in $vars; do
			printf 'export %s=%q\n' "$v" "${!v:-}"
		done
		;;
	*)
		echo "usage: pg_env [--export]" >&2
		return 1
		;;
	esac
}
