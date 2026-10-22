#!/bin/sh
# Regenerate the directory rename fixtures from one real orgfdb index of
# tests/fixtures/dircorpus, so all link ids come from the same database.
#
# Usage: ORGFDB=/path/to/orgfdb scripts/regen-dir-fixtures.sh
set -eu

ROOT=$(cd "$(dirname "$0")/.." && pwd)
ORGFDB=${ORGFDB:-orgfdb}
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
FIXTURES="$ROOT/tests/fixtures"

# The corpus copy gets one extra relative link that passes through the
# directory name. Only the named fixture keeps it; the spec that uses that
# fixture appends the same line before renaming.
LOOP='[[file:../alt/notiz2.org][loop]]'
cp -r "$FIXTURES/dircorpus" "$WORK/source"
printf '%s\n' '* Loop' "$LOOP" >> "$WORK/source/projekte/alt/notiz.org"
ORGFDB="$ORGFDB" "$ROOT/scripts/e2e-index.sh" "$WORK/source" "$WORK" >/dev/null
CORPUS="$WORK/corpus"
CONFIG="$WORK/orgfdb-config.toml"
OLD="$CORPUS/projekte/alt"

query() {
  "$ORGFDB" query --format json --config "$CONFIG" "$1" |
    sed "s#$CORPUS#@CORPUS@#g"
}

without_loop() {
  python3 -I -c '
import json, sys
data = json.load(sys.stdin)
data["results"] = [r for r in data["results"] if r["raw"] != sys.argv[1]]
print(json.dumps(data, indent=2, ensure_ascii=False))' "$LOOP"
}

query "(links (target (files (file-path \"^$OLD/\" :regexp t))))" |
  without_loop > "$FIXTURES/dir-incoming-links.json"
query "(links (source (files (file-path \"^$OLD/\" :regexp t))))" |
  without_loop > "$FIXTURES/dir-outgoing-links.json"
query '(links (and (link-type "file") (link-target "alt")))' \
  > "$FIXTURES/dir-named-links.json"
"$ORGFDB" status --format json --config "$CONFIG" |
  sed "s#$WORK#@CORPUS@#g" > "$FIXTURES/dir-status.json"
