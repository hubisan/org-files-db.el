#!/bin/bash
# Prepare Claude Code on the web sessions for scripts/check-offline.sh: build
# Emacs from the GitHub mirror and clone the development packages, since the
# sandbox blocks the GNU and MELPA package archives that Eask needs.
set -euo pipefail

if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

EMACS_VERSION=30.2
PREFIX="$HOME/.cache/org-files-db-dev"
EMACS_DIR="$PREFIX/emacs-$EMACS_VERSION"
DEPS="$PREFIX/deps"

if [ ! -x "$EMACS_DIR/bin/emacs" ]; then
  src="$PREFIX/src/emacs-$EMACS_VERSION"
  rm -rf "$src"
  mkdir -p "$PREFIX/src"
  git clone -q --depth 1 --branch "emacs-$EMACS_VERSION" \
    https://github.com/emacs-mirror/emacs.git "$src"
  (cd "$src" &&
     ./autogen.sh >/dev/null &&
     ./configure -q --prefix="$EMACS_DIR" --without-all --without-x \
       --without-native-compilation --with-gnutls=no >/dev/null &&
     make -s -j"$(nproc)" >/dev/null &&
     make -s install >/dev/null)
  rm -rf "$src"
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
  echo "export PATH=\"$EMACS_DIR/bin:\$PATH\"" >> "$CLAUDE_ENV_FILE"
  echo "export ORG_FILES_DB_DEPS=\"$DEPS\"" >> "$CLAUDE_ENV_FILE"
fi
