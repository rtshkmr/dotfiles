#!/usr/bin/env bash
#
# Show the values pgenv.sh resolved for the tree you are standing in.
# Usage: pg-env.sh             human-readable summary
#        pg-env.sh --export    export lines for PGPORT/PGDATA/PGDATABASE/PATH;
#                              use as: eval "$(pg-env.sh --export)"
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/pgenv.sh"
pg_env "$@"
