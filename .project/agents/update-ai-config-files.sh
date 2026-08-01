#!/usr/bin/env bash

# -----------------------------------------------------------------------------
# update-ai-config-files.sh
#
# Copies the Org-mode AI config files from an external Git repository into the
# current repository's `.project/` directory.
#
# The script:
#   - detects the root of the current Git repository
#   - clones the external repository into a temporary local folder
#   - copies everything from:
#       external-repo/org-mode/.project/ -> current-repo/.project/
#   - overwrites existing files with the same names
#   - keeps other existing files in `.project/` untouched
#   - removes the temporary clone afterwards
#   - prints a summary of copied/updated files
#
# No files are staged or committed automatically.
# -----------------------------------------------------------------------------

set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT"

TMP=".project-update-tmp"
TARGET=".project"
REPO_URL="https://github.com/hubisan/ai-agents-config.git"
BRANCH="main"
SOURCE="org-mode/.project"

cleanup() {
  rm -rf "$TMP"
}

trap cleanup EXIT

echo "Updating AI config files"
echo "Repository: $REPO_URL"
echo "Branch:     $BRANCH"
echo "Source:     $SOURCE"
echo "Target:     $TARGET"
echo

rm -rf "$TMP"
mkdir -p "$TARGET"

echo "Fetching latest files..."
git clone -q --depth=1 --branch "$BRANCH" "$REPO_URL" "$TMP"

if [ ! -d "$TMP/$SOURCE" ]; then
  echo "Error: Source directory not found: $SOURCE"
  exit 1
fi

echo "Copying files..."
RSYNC_OUTPUT="$(
  rsync -a --itemize-changes "$TMP/$SOURCE/" "$TARGET/" \
    | awk '
      /^[^.]/{ print "  " $2 }
    '
)"

if [ -n "$RSYNC_OUTPUT" ]; then
  echo
  echo "Updated files:"
  echo "$RSYNC_OUTPUT"
else
  echo
  echo "No file changes detected."
fi

echo
echo "Done. No files were staged or committed."
echo
echo "Current Git status:"
echo

git status --short
