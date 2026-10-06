#!/usr/bin/env bash
source "$(dirname "${BASH_SOURCE[0]}")/pgenv.sh"
psql -X -Atc "
  select pid, backend_type, coalesce(state,''), left(coalesce(query,''), 50)
  from pg_stat_activity
  where pid <> pg_backend_pid()
  order by backend_type, pid"
