#!/usr/bin/env bash
# pg-primetree.sh -- prime a fresh checkout or worktree so that the build, clangd and
# (optionally) a private cluster all point at *this* tree.
#
# Run it from inside the tree you want to prime (main or any worktree). In order:
#   1. pg-setup.sh    meson setup          -> $PG_BUILD, writes compile_commands.json
#   2. pg-build.sh    ninja install        -> $PG_PREFIX, generates headers (nodetags.h, ...)
#   3. pg-clangd.sh   .clangd for this tree, pointing at $PG_BUILD
#   4. pg-init.sh     only with --init: private cluster in $PGDATA on $PGPORT
#
# It only orchestrates the other scripts; each of them still works on its own.
# Part of the pg-* script set; sources pgenv.sh for the per-tree paths.
set -euo pipefail
d="$(dirname "${BASH_SOURCE[0]}")"
source "$d/pgenv.sh"

usage() {
  cat <<'EOF'
Usage: pg-primetree.sh [--wipe] [--init] [--check] [-h]

  (no args)   configure + build + write .clangd for the tree you are standing in
  --wipe      start the build dir from scratch (pg-setup.sh --wipe). This also deletes
              clangd's background index, which lives inside the build dir, so Postgres
              is re-indexed the next time the editor opens a file.
  --init      also create this tree's cluster (pg-init.sh); skipped if one already exists
  --check     run `pg-clangd.sh --check` at the end (header + postgres.c sanity check)
  -h, --help  this text
EOF
}

die()  { echo "error: $*" >&2; exit 1; }
warn() { echo "warn:  $*" >&2; }
info() { echo "==> $*"; }

# ---- args -------------------------------------------------------------------
WIPE=0; INIT=0; CHECK=0
while (($#)); do
  case $1 in
    --wipe)  WIPE=1 ;;
    --init)  INIT=1 ;;
    --check) CHECK=1 ;;
    -h|--help) usage; exit 0 ;;
    *) echo "unknown argument: $1" >&2; usage >&2; exit 2 ;;
  esac
  shift
done

# ---- sanity checks on the tree -------------------------------------------------
[[ -f "$PG_SRC/meson.build" && -d "$PG_SRC/src/backend" ]] \
  || die "$PG_SRC does not look like a PostgreSQL checkout (no meson.build / src/backend)"

if [[ -z $(git rev-parse --show-toplevel 2>/dev/null || true) ]]; then
  warn "not inside a git checkout; pgenv.sh fell back to PG_SRC=$PG_SRC"
fi

# The first entry of `git worktree list` is the main worktree.
main_dir=$(git -C "$PG_SRC" worktree list --porcelain 2>/dev/null | awk '/^worktree /{print substr($0, 10); exit}' || true)
if [[ -n "$main_dir" && "$PG_SRC" != "$main_dir" && "$PG_SRC/" == "$main_dir"/* ]]; then
  warn "$PG_SRC is nested inside $main_dir."
  warn "clangd searches parent directories for .clangd, so main's .clangd and lsp-mode's"
  warn "workspace root can capture this tree. Prefer a sibling directory:"
  warn "  git worktree move \"$PG_SRC\" \"$(dirname "$main_dir")/$PG_WORKTREE_ID\""
fi

info "worktree: $PG_WORKTREE_ID"
info "source:  $PG_SRC"
info "build:   $PG_BUILD"
info "prefix:  $PG_PREFIX"
info "data:    $PGDATA   (port $PGPORT)"

steps=3
((INIT)) && steps=4
n=0
step() { n=$((n + 1)); info "[$n/$steps] $*"; }

# ---- 1. configure ------------------------------------------------------------
setup_args=()
if ((WIPE)); then setup_args+=(--wipe); fi
step "pg-setup.sh ${setup_args[*]-}"
"$d/pg-setup.sh" ${setup_args[@]+"${setup_args[@]}"}

# ---- 2. build ----------------------------------------------------------------
step "pg-build.sh"
"$d/pg-build.sh"

# ---- 3. clangd ---------------------------------------------------------------
clangd_args=()
if ((CHECK)); then clangd_args+=(--check); fi
step "pg-clangd.sh ${clangd_args[*]-}"
"$d/pg-clangd.sh" ${clangd_args[@]+"${clangd_args[@]}"}

# ---- 4. cluster (optional) ---------------------------------------------------
if ((INIT)); then
  step "pg-init.sh"
  if [[ -f "$PGDATA/PG_VERSION" ]]; then
    info "cluster already exists at $PGDATA; leaving it alone (pg-reset.sh recreates it)"
  else
    "$d/pg-init.sh"
  fi
fi

# ---- next steps --------------------------------------------------------------
echo
info "worktree '$PG_WORKTREE_ID' is primed."
echo "In Emacs: open a file from $PG_SRC, then M-x lsp-workspace-restart."
echo "The first background index of Postgres takes a few minutes; until it finishes,"
echo "M-. on a function can stop at its declaration in the header."
echo "Check which tree the buffer belongs to with: M-: (lsp-workspace-root)"
if ((INIT)); then
  echo "Start this worktree's server with: pg-start.sh   (port $PGPORT)"
fi
