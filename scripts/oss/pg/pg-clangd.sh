#!/usr/bin/env bash
# pg-clangd.sh -- generate a local .clangd for the PostgreSQL tree and sanity-check clangd.
#
# Toolchain policy: gcc on Linux, Apple clang on macOS (override with PG_CC).
# clangd is still the language server either way (gcc has no LSP), so this script
# makes sure clangd reads the *same* compile database the real build produced,
# warns if the build compiler isn't the one you intended, and strips gcc-only
# flags that clangd can't parse.
#
# Part of the pg-* script set; sources pgenv.sh for PG_SRC / PG_BUILD / PG_PREFIX.
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/pgenv.sh"

usage() {
  cat <<'EOF'
Usage: pg-clangd.sh [--check [FILE]] [-h]

  (no args)     write $PG_SRC/.clangd for the existing meson build dir ($PG_BUILD)
                and verify the build compiler matches the intended toolchain
                (gcc on Linux, clang on macOS; override with PG_CC=...)
  --check [F]   also run `clangd --check` on F (default: rel.h and postgres.c);
                full output is kept in /tmp/pg-clangd-check-*.log
  -h, --help    this text
EOF
}

die()  { echo "error: $*" >&2; exit 1; }
warn() { echo "warn:  $*" >&2; }
info() { echo "==> $*"; }

# ---- args -------------------------------------------------------------------
DO_CHECK=0
CHECK_FILE=""
while (($#)); do
  case $1 in
    --check)
      DO_CHECK=1
      if [[ -n ${2:-} && $2 != --* ]]; then CHECK_FILE=$2; shift; fi
      ;;
    -h|--help) usage; exit 0 ;;
    *) echo "unknown argument: $1" >&2; usage >&2; exit 2 ;;
  esac
  shift
done

# ---- OS detection -----------------------------------------------------------
# OS is one of: macos | arch | linux | other
detect_os() {
  case "$(uname -s)" in
    Darwin) OS=macos ;;
    Linux)
      OS=linux
      if [[ -r /etc/os-release ]]; then
        local ids
        ids=$( . /etc/os-release; echo " ${ID:-} ${ID_LIKE:-} " )
        case $ids in *" arch "*) OS=arch ;; esac
      fi
      ;;
    *) OS=other ;;
  esac
}

clangd_hint() {
  case $OS in
    macos) echo "brew install llvm   (keg-only: clangd lands in $(brew --prefix llvm 2>/dev/null || echo '<llvm prefix>')/bin)" ;;
    arch)  echo "sudo pacman -S clang   (the clang package should include clangd)" ;;
    linux) echo "install clangd with your package manager (e.g. apt install clangd, dnf install clang-tools-extra)" ;;
    *)     echo "install clangd (part of LLVM)" ;;
  esac
}

cc_hint() {
  case $OS in
    macos) echo "xcode-select --install" ;;
    arch)  echo "sudo pacman -S gcc   (or base-devel)" ;;
    *)     echo "install your distro's gcc package (e.g. build-essential)" ;;
  esac
}

# Prints the clangd path; returns 1 if none found.
find_clangd() {
  local c
  if c=$(command -v clangd 2>/dev/null); then echo "$c"; return 0; fi
  if [[ $OS == macos ]]; then
    if command -v brew >/dev/null 2>&1; then
      c="$(brew --prefix llvm 2>/dev/null || true)/bin/clangd"
      if [[ -x $c ]]; then echo "$c"; return 0; fi
    fi
    if c=$(xcrun -f clangd 2>/dev/null) && [[ -x $c ]]; then echo "$c"; return 0; fi
  fi
  return 1
}

# $1 = path to a compiler executable -> prints gcc | clang | unknown
compiler_family() {
  local v
  v=$("$1" --version 2>/dev/null | head -n 3 || true)
  if grep -qi clang <<<"$v"; then echo clang
  elif grep -qiE 'gcc|free software foundation' <<<"$v"; then echo gcc
  else echo unknown; fi
}

detect_os
info "platform: $OS"

CLANGD=$(find_clangd) || die "clangd not found. $(clangd_hint)"
info "clangd:   $CLANGD ($("$CLANGD" --version 2>/dev/null | head -n1 || echo 'version unknown'))"

[[ -d $PG_SRC ]] || die "PG_SRC=$PG_SRC does not exist (set it in pgenv.sh)"

# ---- intended build compiler ---------------------------------------------------
if [[ -n ${PG_CC:-} ]]; then WANT_CC=$PG_CC
elif [[ $OS == macos ]]; then WANT_CC=clang
else WANT_CC=gcc
fi

want_family=unknown
if want_path=$(command -v "$WANT_CC" 2>/dev/null); then
  want_family=$(compiler_family "$want_path")
  info "intended compiler: $WANT_CC ($want_family)"
else
  warn "intended compiler '$WANT_CC' not found. $(cc_hint)"
fi

# ---- compile database checks -------------------------------------------------
CDB="$PG_BUILD/compile_commands.json"
[[ -f $CDB ]] || die "$CDB not found. Run pg-setup.sh (meson writes it)."

db_cmd=""
if command -v jq >/dev/null 2>&1; then
  db_cmd=$(jq -r '.[0].command // ((.[0].arguments // []) | join(" "))' "$CDB" 2>/dev/null || true)
elif command -v python3 >/dev/null 2>&1; then
  db_cmd=$(python3 - "$CDB" <<'PY' 2>/dev/null || true
import json, sys
e = json.load(open(sys.argv[1]))[0]
print(e.get("command") or " ".join(e.get("arguments", [])))
PY
)
fi

db_cc=""
db_family=unknown
if [[ -n $db_cmd ]]; then
  read -ra toks <<<"$db_cmd"
  db_cc=${toks[0]}
  if [[ $(basename "$db_cc") == ccache && ${#toks[@]} -ge 2 ]]; then db_cc=${toks[1]}; fi
  if resolved=$(command -v "$db_cc" 2>/dev/null); then
    db_family=$(compiler_family "$resolved")
  fi
fi

if [[ $db_family == unknown ]]; then
  warn "could not determine the build compiler from $CDB (needs jq or python3, and the compiler on PATH)"
elif [[ $want_family != unknown && $db_family != "$want_family" ]]; then
  warn "the build dir was configured with $db_family ($db_cc), but you intended $want_family ($WANT_CC)."
  warn "meson reads CC only on the first configure. To switch: PG_CC=$WANT_CC pg-setup.sh --wipe && pg-build.sh   (full rebuild)"
else
  info "build compiler: $db_cc ($db_family) -- matches the intended toolchain"
fi

# Compiler paths clangd may execute to learn gcc's system include dirs and predefined macros.
qd=""
if [[ $db_family == gcc ]]; then
  qd=$(printf '%s\n' "$(command -v "$db_cc" 2>/dev/null || true)" \
                     "$(command -v cc 2>/dev/null || true)" \
                     "$(command -v "$WANT_CC" 2>/dev/null || true)" \
       | awk 'NF && !seen[$0]++' | paste -sd, -)
fi

if [[ -z $(find "$PG_BUILD" -name nodetags.h 2>/dev/null | head -n1 || true) ]]; then
  warn "generated headers (e.g. nodetags.h) not found under $PG_BUILD -- run the build (pg-build.sh) first."
fi

# ---- write .clangd -----------------------------------------------------------
# gcc-only flags from the Postgres meson build that clangd may reject. Best-effort:
# `pg-clangd.sh --check` shows any that still slip through; add them here.
gcc_only_flags='-fexcess-precision=*, -Wshadow=*, -Wimplicit-fallthrough=*, -Wno-format-truncation, -Wno-stringop-truncation, -fno-aggressive-loop-optimizations, -fmax-errors=*'

render_clangd() {
  echo "# Generated by pg-clangd.sh -- local only (excluded via .git/info/exclude). Safe to regenerate."
  echo "CompileFlags:"
  echo "  CompilationDatabase: \"$PG_BUILD\""
  if [[ $db_family == gcc ]]; then
    echo "  # gcc-only flags from the build that clangd doesn't understand (best-effort list):"
    echo "  Remove: [$gcc_only_flags]"
  fi
  cat <<'EOF'
---
# Backend headers are not self-contained: by project convention every .c file
# includes postgres.h first, and the headers rely on it for bool, Oid, int16,
# MemoryContext and friends. Pre-include it when clangd parses a header alone.
# A header has no compile command of its own, so clangd borrows one from a
# nearby file; drop -DFRONTEND in case it borrowed a frontend command, because
# postgres.h refuses to compile under FRONTEND.
If:
  # For a project .clangd, clangd matches these regexes against the path *relative to
  # this file's directory* (no leading slash), so the optional (.*/)? lets the pattern
  # work for relative and absolute paths alike.
  PathMatch: '(.*/)?src/include/.*\.h'
  PathExclude: '(.*/)?src/include/(fe_utils/.*|postgres_fe\.h)'
CompileFlags:
  Add: [-include, postgres.h]
  Remove: [-DFRONTEND]
---
# Postgres headers are deliberately not include-what-you-use clean.
Diagnostics:
  UnusedIncludes: None
  MissingIncludes: None

# ----
# Generated by pg-clangd.sh -- local only (excluded via .git/info/exclude). Safe to regenerate.
#
# Why this file exists
#   Postgres is built with gcc (meson), but clangd is clang-based. Left alone it
#   shows errors that come from a toolchain mismatch or from Postgres' header
#   conventions, not from real bugs. This file makes clangd see the code the way
#   the build does.
#
#   1. CompilationDatabase -> the meson build dir. That is where meson writes
#      compile_commands.json, so every .c file gets its real flags and -I paths.
#   2. Remove: gcc-only flags from that database (-fexcess-precision, -Wshadow=*,
#      ...). clangd would reject them. Best-effort list; add to it if --check
#      shows a new "unknown argument" warning.
#   3. Pre-include postgres.h for headers. Backend headers are not self-contained:
#      by convention every .c file includes postgres.h first, and headers rely on
#      it for bool, Oid, int16, MemoryContext and so on. A header has no compile
#      command of its own, so clangd borrows one from a nearby .c file and then
#      parses the header alone, which gives bogus "unknown type name" errors.
#      -include postgres.h fixes that.
#        - Frontend headers (fe_utils/*, postgres_fe.h) are excluded, because
#          postgres.h refuses to compile under FRONTEND.
#        - -DFRONTEND is removed in case a frontend command was borrowed.
#        - The path regexes are '(.*/)?src/include/...' because clangd matches a
#          project .clangd against the path *relative to this file's directory*
#          (no leading slash). An earlier '.*/src/include/...' never matched.
#   4. Diagnostics: Postgres headers are deliberately not include-what-you-use
#      clean, so the unused/missing-include hints are turned off.
#
# Editor side (Doom config.el), needed so clangd can ask gcc for its system
# include dirs and predefined macros:
#   (after! lsp-clangd
#     (add-to-list 'lsp-clients-clangd-args "--query-driver=/usr/bin/cc,/usr/bin/gcc"))
#
# Verifying: pg-clangd.sh --check runs clangd on a header and a .c file. Ignore
#   "tweak: DefineOutline ==> FAIL" and "Failed to resolve URI" lines; they are
#   refactoring-feature noise. What matters is "no compiler diagnostics found",
#   and for headers, that the logged command ends in "-include postgres.h".

EOF
}

CLANGD_FILE="$PG_SRC/.clangd"
tmp=$(mktemp)
render_clangd > "$tmp"
chmod 644 "$tmp"
if [[ -f $CLANGD_FILE ]] && ! cmp -s "$tmp" "$CLANGD_FILE"; then
  cp "$CLANGD_FILE" "$CLANGD_FILE.bak"
  info "existing .clangd differed; saved as .clangd.bak"
fi
mv "$tmp" "$CLANGD_FILE"
info "wrote $CLANGD_FILE (compile db: $PG_BUILD)"

# ---- keep local files out of git without touching tracked files --------------
exclude_file=$(git -C "$PG_SRC" rev-parse --path-format=absolute --git-path info/exclude 2>/dev/null || true)
ensure_excluded() {
  [[ -n $exclude_file ]] || return 0
  mkdir -p "$(dirname "$exclude_file")"
  touch "$exclude_file"
  grep -qxF -- "$1" "$exclude_file" || echo "$1" >> "$exclude_file"
}
if [[ -n $exclude_file ]]; then
  for pat in ".clangd" ".clangd.bak" "compile_commands.json" ".cache/"; do
    ensure_excluded "$pat"
  done
else
  warn "$PG_SRC is not a git checkout (or git is too old for --path-format); skipped .git/info/exclude"
fi

# Convenience symlink for tools that look for compile_commands.json at the root.
link="$PG_SRC/compile_commands.json"
if [[ ! -e $link || -L $link ]]; then
  ln -sfn "$CDB" "$link"
fi

# ---- optional: clangd --check ------------------------------------------------
if ((DO_CHECK)); then
  if [[ -n $CHECK_FILE ]]; then
    files=("$CHECK_FILE")
  else
    files=("src/include/utils/rel.h" "src/backend/tcop/postgres.c")
  fi
  # Mirror the editor: pass --query-driver when the build is gcc.
  check_args=()
  if [[ -n $qd ]]; then check_args+=("--query-driver=$qd"); fi
  for f in "${files[@]}"; do
    log="/tmp/pg-clangd-check-$(basename "$f").log"
    info "clangd --check=$f   (full log: $log)"
    (cd "$PG_SRC" && "$CLANGD" ${check_args[@]+"${check_args[@]}"} --check="$f" >"$log" 2>&1) || true

    # Which compile command did clangd use? (for headers it is borrowed from a nearby file).
    # Long lines are shortened in the middle: the end is where .clangd's added flags show up.
    grep -m2 -E 'Compile command|Loaded compilation database' "$log" \
      | awk '{ if (length($0) > 300) print substr($0, 1, 110) " ... " substr($0, length($0) - 179); else print }' || true
    cmdline=$(grep -m1 'Compile command' "$log" || true)

    # `clangd --check` counts every failed editor refactoring ("tweak") as an error, and in
    # macro-heavy Postgres code that is most of the count. Separate that noise from real
    # compiler diagnostics, which are logged as: E[..] [diag_name] Line N: message
    total=$(grep -c '^E\[' "$log" || true)
    noise=$(grep '^E\[' "$log" | grep -cE 'tweak:|Failed to resolve URI' || true)
    info "$(grep 'All checks completed' "$log" | tail -n1 | sed 's/^[^ ]* //' || true)"
    info "E-level log lines: $total; $noise of them are refactoring-tweak/URI noise, not compile errors"
    real=$(grep -E '^[EW]\[' "$log" | grep -vE 'tweak:|Failed to resolve URI' || true)
    if [[ -n $real ]]; then
      echo "diagnostics that look real (first 40):"
      echo "$real" | head -n 40 | cut -c1-240
      if [[ $f == *src/include/*.h && $cmdline != *postgres.h* ]]; then
        warn "the command clangd used for this header has no '-include postgres.h': the header rule in .clangd isn't matching it."
      fi
    else
      echo "no compiler diagnostics found in the log."
    fi
  done
fi

# ---- next steps ---------------------------------------------------------------
echo
if [[ $(command -v clangd 2>/dev/null || true) != "$CLANGD" ]]; then
  echo "Emacs can't find clangd on its PATH? Add to config.el:"
  echo "  (setq lsp-clients-clangd-executable \"$CLANGD\")"
  echo
fi
if [[ $db_family == gcc && -n $qd ]]; then
  echo "The build is gcc but clangd is clang-based. To let it query gcc for system include"
  echo "dirs and predefined macros, add to Doom's config.el:"
  echo "  (after! lsp-clangd"
  echo "    (add-to-list 'lsp-clients-clangd-args \"--query-driver=$qd\"))"
  echo
fi
echo "In Emacs: M-x lsp-workspace-restart, then reopen src/include/utils/rel.h."
