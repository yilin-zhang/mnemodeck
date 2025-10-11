#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"
FSRS_DIR="$(./scripts/check-deps.sh)"

emacs --batch -L "$FSRS_DIR" -L . -L tests -l tests/mnemodeck-test.el -f ert-run-tests-batch-and-exit
