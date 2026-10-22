#!/bin/sh
# Compile, lint and test without Eask, using development packages from
# $ORG_FILES_DB_DEPS (cloned by .claude/hooks/session-start.sh). Runs the
# checks of `make ci': byte-compile with warnings as errors, package-lint,
# checkdoc, indentation, relint and Buttercup with Eask's abort on `error'.
set -eu

EMACS=${EMACS:-emacs}
DEPS=${ORG_FILES_DB_DEPS:-$HOME/.cache/org-files-db-dev/deps}
ROOT=$(cd "$(dirname "$0")/.." && pwd)

if [ ! -d "$DEPS/emacs-buttercup" ]; then
  echo "Missing development packages in $DEPS; run .claude/hooks/session-start.sh" >&2
  exit 1
fi

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
cp -r "$ROOT/lisp" "$ROOT/tests" "$ROOT/scripts" "$work"
cd "$work"

set -- -L lisp -L tests
for dep in compat embark emacs-buttercup package-lint relint xr; do
  set -- "$@" -L "$DEPS/$dep"
done

step() { printf '\n>> %s\n' "$1"; }

step compile
"$EMACS" -Q --batch "$@" \
  --eval '(setq byte-compile-error-on-warn t)' \
  -f batch-byte-compile lisp/*.el
rm -f lisp/*.elc

step package-lint
"$EMACS" -Q --batch "$@" -l package-lint \
  --eval '(setq package-lint-main-file "lisp/org-files-db.el" package-lint-batch-fail-on-warnings nil)' \
  -f package-lint-batch-and-exit lisp/*.el

step checkdoc
"$EMACS" -Q --batch "$@" -l scripts/check-offline.el \
  -f org-files-db-check-offline-checkdoc

step indent
mkdir indent-check && cp -r lisp tests indent-check
"$EMACS" -Q --batch "$@" -l scripts/format.el \
  $(find indent-check -name '*.el' | sort) >/dev/null 2>&1
diff -ru lisp indent-check/lisp && diff -ru tests indent-check/tests

step relint
"$EMACS" -Q --batch "$@" -l relint -f relint-batch lisp/*.el

step test
"$EMACS" -Q --batch "$@" -l buttercup -l scripts/check-offline.el \
  -f org-files-db-check-offline-abort-on-error \
  -f buttercup-run-discover "$work/tests"
