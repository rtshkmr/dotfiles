#!/usr/bin/env bash
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/pgenv.sh"
pg_require_install
pg_ctl -D "$PGDATA" -m fast stop
