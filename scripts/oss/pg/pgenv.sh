#!/usr/bin/env sh
#
export PG_SRC="${PG_SRC:-$HOME/projects/pg}" # your git checkout
export PG_BUILD="$PG_SRC/build"
export PG_PREFIX="$HOME/pgsql-dev"
export PGDATA="$PG_PREFIX/data"
export PGPORT=5433 # avoid clashing with a system/Homebrew instance on 5432
export PGDATABASE=postgres
export PATH="$PG_PREFIX/bin:$PATH"

icu="$(brew --prefix icu4c 2>/dev/null || true)" # may need a versioned name, e.g. icu4c@77
[[ -n "$icu" ]] && export PKG_CONFIG_PATH="$icu/lib/pkgconfig${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"
