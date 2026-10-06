#!/usr/bin/env bash
set -euo pipefail
d="$(dirname "${BASH_SOURCE[0]}")"
source "$d/pgenv.sh"
"$d/pg-stop.sh" || true
rm -rf "${PGDATA:?}"
"$d/pg-init.sh"
"$d/pg-start.sh"
