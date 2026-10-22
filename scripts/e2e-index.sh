#!/bin/sh
# Index a copy of an Org directory with the real orgfdb for end-to-end checks.
#
# Usage: scripts/e2e-index.sh [CORPUS] [WORKDIR]
#
# CORPUS defaults to tests/fixtures/dircorpus. The copy, the orgfdb
# configuration and the database go to WORKDIR (a new temporary directory by
# default). ORGFDB names the orgfdb executable (default: orgfdb). Prints the
# configuration path and the Emacs Lisp to point org-files-db at it.
set -eu

ROOT=$(cd "$(dirname "$0")/.." && pwd)
CORPUS=${1:-$ROOT/tests/fixtures/dircorpus}
WORK=${2:-$(mktemp -d)}
ORGFDB=${ORGFDB:-orgfdb}

mkdir -p "$WORK"
rm -rf "$WORK/corpus"
cp -r "$CORPUS" "$WORK/corpus"
cat > "$WORK/orgfdb-config.toml" <<TOML
db_path = "$WORK/db.sqlite"

[[dirs]]
path = "$WORK/corpus"
recursive = true
TOML
"$ORGFDB" rebuild --config "$WORK/orgfdb-config.toml" >/dev/null

cat <<ELISP
Corpus: $WORK/corpus
Config: $WORK/orgfdb-config.toml

(setq org-files-db-executable "$(command -v "$ORGFDB")"
      org-files-db-configs '(("e2e" . "$WORK/orgfdb-config.toml"))
      org-files-db-default-config "e2e")
ELISP
