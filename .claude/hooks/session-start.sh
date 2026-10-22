#!/bin/bash
# Prepare Claude Code on the web sessions for scripts/check-offline.sh: install
# Emacs from Ubuntu and clone the development packages from GitHub, since the
# sandbox blocks the GNU and MELPA package archives that Eask needs.
set -euo pipefail

if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

PREFIX="$HOME/.cache/org-files-db-dev"
DEPS="$PREFIX/deps"

# Ubuntu 24.04 ships Emacs 29.3, the oldest version the package supports.
if ! command -v emacs >/dev/null; then
  apt-get update -qq >/dev/null
  apt-get install -y -qq emacs-nox >/dev/null
fi

mkdir -p "$DEPS"
for repo in jorgenschaefer/emacs-buttercup oantolin/embark emacs-compat/compat \
            purcell/package-lint mattiase/relint mattiase/xr; do
  dir="$DEPS/${repo#*/}"
  if [ -d "$dir/.git" ]; then
    git -C "$dir" pull -q --ff-only || true
  else
    git clone -q --depth 1 "https://github.com/$repo.git" "$dir"
  fi
done

if [ -n "${CLAUDE_ENV_FILE:-}" ]; then
  echo "export ORG_FILES_DB_DEPS=\"$DEPS\"" >> "$CLAUDE_ENV_FILE"
fi
