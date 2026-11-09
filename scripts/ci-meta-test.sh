#!/usr/bin/env bash
# Compat: invariantes comunitarias (antes meta-repo).
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
exec bash "$ROOT/scripts/ci-community-test.sh"
