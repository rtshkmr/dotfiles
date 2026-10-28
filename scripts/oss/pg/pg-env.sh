#!/usr/bin/env bash
# Print the pg environment for the worktree you're in.
#   pg-env.sh            aligned listing
#   pg-env.sh --export   eval-able export lines
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/pgenv.sh"
pg_env "$@"
