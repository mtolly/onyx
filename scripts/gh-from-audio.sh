#!/usr/bin/env bash
# Wrap onyx-cli to build a Clone Hero / Guitar Hero-style song from audio.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
ONYX="${ONYX:-$ROOT/haskell/.stack-work/install/aarch64-osx/*/ghc-*/bin/onyx-cli}"

if ! command -v onyx-cli >/dev/null 2>&1; then
  if compgen -G "$ONYX" >/dev/null; then
    ONYX="$(ls -1t $ROOT/haskell/.stack-work/install/*/ghc-*/bin/onyx-cli 2>/dev/null | head -1)"
  elif [[ -x "$ROOT/haskell/.stack-work/install/aarch64-osx"/*/ghc-*/bin/onyx ]]; then
    ONYX="$(ls -1t $ROOT/haskell/.stack-work/install/*/ghc-*/bin/onyx 2>/dev/null | head -1)"
  else
    echo "onyx-cli not found. Build with: cd haskell && ./stack-local build onyx-cli" >&2
    exit 1
  fi
else
  ONYX=onyx-cli
fi

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 audio.wav [--to output.zip] [--target ps|gh5]" >&2
  exit 1
fi

exec "$ONYX" from-audio "$@"
