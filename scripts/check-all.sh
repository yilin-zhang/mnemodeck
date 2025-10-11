#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"
FSRS_DIR="$(./scripts/check-deps.sh)"

echo "[1/5] Check parentheses"
emacs --batch --eval '(with-temp-buffer (insert-file-contents "mnemodeck.el") (check-parens))'

echo "[2/5] Byte compile"
emacs --batch -L "$FSRS_DIR" -L . -f batch-byte-compile mnemodeck.el

echo "[3/5] Remove generated .elc"
find . -name '*.elc' -delete

echo "[4/5] Checkdoc"
./scripts/check-checkdoc.sh

echo "[5/5] Package lint + tests"
./scripts/check-package-lint.sh
./scripts/check-ert.sh

echo "All checks passed."
