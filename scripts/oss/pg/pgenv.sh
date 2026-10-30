#!/usr/bin/env bash
#
# Sourced by every pg-*.sh script; not meant to be run directly.
# Needs bash (uses [[ ]]) and stays bash-3.2 compatible for stock macOS.
#
# Tree-aware: everything derives from the git checkout you are standing in (main or a
# linked worktree), so each tree gets its own build dir, install prefix, data dir and port.
#
#   PG_SRC          git toplevel of the current directory (falls back to ~/Projects/pg
#                   outside a checkout). Override: PG_SRC=/path/to/tree pg-setup.sh
#   PG_WORKTREE_ID  "main" for the main worktree, otherwise the worktree directory's name
#   PG_BUILD        $PG_SRC/build
#   PG_PREFIX       main: ~/pgsql-dev        linked: ~/pgsql-dev-worktrees/<id>
#   PGDATA          $PG_PREFIX/data
#   PGPORT          main: 5433               linked: 5434..5533, hashed from <id>
#   PG_CC           clang on macOS, gcc on Linux (override: PG_CC=gcc-14 pg-setup.sh --wipe)
#
# Only PG_SRC, PG_CC and PG_DEBUGGER honour a value already in the environment. The
# derived values are recomputed on every source, so a stale PGPORT/PGDATA exported by
# another tree's shell (or by pg-env.sh --export) can never leak into this tree.
# Run pg-env.sh (or call pg_env) to print what this tree resolved to.

_pg_main_src="$HOME/Projects/pg" # only used when not inside any git checkout
_pg_top=$(git rev-parse --show-toplevel 2>/dev/null || true)
export PG_SRC="${PG_SRC:-${_pg_top:-$_pg_main_src}}"
export PG_BUILD="$PG_SRC/build"

# Absolute, symlink-free path of a `git rev-parse <option>` directory, or nothing.
_pg_realdir() {
	local p
	p=$(cd "$PG_SRC" 2>/dev/null && git rev-parse "$1" 2>/dev/null) || return 0
	[[ -n "$p" ]] || return 0
	(cd "$PG_SRC" && cd "$p" && pwd -P) 2>/dev/null || true
}

# The main worktree's git dir *is* the common git dir; a linked worktree has its own.
_pg_gitdir=$(_pg_realdir --git-dir)
_pg_commondir=$(_pg_realdir --git-common-dir)
if [[ -z "$_pg_gitdir" || "$_pg_gitdir" == "$_pg_commondir" ]]; then
	PG_WORKTREE_ID=main
else
	PG_WORKTREE_ID=$(basename "$PG_SRC")
fi
export PG_WORKTREE_ID

if [[ "$PG_WORKTREE_ID" == main ]]; then
	PG_PREFIX="$HOME/pgsql-dev"
	PGPORT=5433 # avoid clashing with a system/Homebrew/pacman instance on 5432
else
	PG_PREFIX="$HOME/pgsql-dev-worktrees/$PG_WORKTREE_ID"
	# stable per-worktree port in 5434..5533
	PGPORT=$((5434 + $(printf '%s' "$PG_WORKTREE_ID" | cksum | cut -d' ' -f1) % 100))
fi
export PG_PREFIX PGPORT
export PGDATA="$PG_PREFIX/data"
export PGDATABASE=postgres
export PATH="$PG_PREFIX/bin:$PATH"

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
# PG_DEBUGGER and PG_CC can be overridden from the caller, e.g. PG_DEBUGGER=lldb on Arch.
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

# --- guard rails ---------------------------------------------------------------
# Scripts that talk to this worktree's server need *its* install first. Without one, PATH
# silently falls through to some other postgres (system, or another worktree's).
pg_require_install() {
	[[ -x "$PG_PREFIX/bin/pg_ctl" ]] && return 0
	echo "No install for worktree '$PG_WORKTREE_ID' at $PG_PREFIX (run pg-setup.sh && pg-build.sh)." >&2
	return 1
}

# Policy: no compiler caches (ccache, sccache). pg-setup.sh refuses a cache wrapper as
# PG_CC; this checks what an existing build dir was actually configured with, because a
# reconfigure keeps the compiler chosen at first configure. Silent if nothing is configured.
pg_require_no_compiler_cache() {
	local cdb="$PG_BUILD/compile_commands.json" first cc resolved real
	[[ -f "$cdb" ]] || return 0
	first=$(grep -m1 '"command"' "$cdb" | sed -e 's/^[^:]*:[[:space:]]*"//') || true
	cc=${first%% *}
	[[ -n "$cc" ]] || return 0
	resolved=$(command -v "$cc" 2>/dev/null || true)
	real=""
	if [[ -n "$resolved" ]]; then real=$(readlink -f "$resolved" 2>/dev/null || true); fi
	case "$cc $resolved $real" in
	*ccache*)
		echo "error: the build dir $PG_BUILD was configured with a compiler cache (compiler '$cc'${real:+ -> '$real'})." >&2
		echo "       Reconfigure with the real compiler:  pg-setup.sh --wipe" >&2
		return 1
		;;
	esac
}

# --- debugger helpers --------------------------------------------------------
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
	echo "      Until reboot: sudo sysctl kernel.yama.ptrace_scope=0" >&2
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

# --- show the resolved environment -------------------------------------------
# Handy for the values that are derived per tree (PGPORT, PG_PREFIX, ...).
#   pg_env            human-readable summary, plus build/install/cluster status
#   pg_env --export   `export` lines for the libpq-facing values only (PGPORT, PGDATA,
#                     PGDATABASE, PATH), to point psql etc. in your shell at this tree:
#                       eval "$(pg-env.sh --export)"
pg_env() {
	local v
	local vars="PG_WORKTREE_ID PG_SRC PG_BUILD PG_PREFIX PGDATA PGPORT PGDATABASE PG_CC PG_OS PG_DEBUGGER"
	case "${1:-}" in
	--export)
		for v in PGPORT PGDATA PGDATABASE PATH; do printf 'export %s=%q\n' "$v" "${!v}"; done
		;;
	"")
		for v in $vars; do printf '%-15s %s\n' "$v" "${!v}"; done
		printf '%-15s %s\n' "build dir" \
			"$([[ -f "$PG_BUILD/compile_commands.json" ]] && echo configured || echo 'not configured (run pg-setup.sh)')"
		printf '%-15s %s\n' "install" \
			"$([[ -x "$PG_PREFIX/bin/pg_ctl" ]] && echo present || echo 'missing (run pg-build.sh)')"
		printf '%-15s %s\n' "cluster" \
			"$([[ -f "$PGDATA/PG_VERSION" ]] && echo initialized || echo 'not initialized (run pg-init.sh)')"
		;;
	*) echo "Usage: pg_env [--export]" >&2; return 2 ;;
	esac
}
