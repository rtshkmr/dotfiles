#!/usr/bin/env bash
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/pgenv.sh"
pg_require_install
exec psql "$@"
