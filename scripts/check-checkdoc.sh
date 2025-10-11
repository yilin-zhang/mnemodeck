#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

emacs --batch --eval "(progn
  (require 'checkdoc)
  (unless (checkdoc-file \"mnemodeck.el\")
    (princ \"checkdoc failed\\n\")
    (kill-emacs 1)))"
