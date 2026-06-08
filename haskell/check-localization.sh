#!/usr/bin/env bash
# Cross-check Fluent (.ftl) localization keys against their usage in the
# onyx-exe-toolkit source. Reports:
#   - keys referenced in code via l10n / l10nArgs / l10nFull but missing from
#     the .ftl file (these silently render as "{key}" at runtime), and
#   - keys defined in the .ftl file that are never referenced in code.
#
# Usage: ./check-localization.sh
# Exits non-zero if any referenced key is missing from the .ftl file.

set -euo pipefail

# Run relative to this script's directory (the haskell/ root).
cd "$(dirname "$0")"

FTL=resources/localization/en/main.ftl
SRC_GLOBS=(
  packages/onyx-exe-toolkit/src/Onyx/GUI.hs
  packages/onyx-exe-toolkit/src/Onyx/GUI/*.hs
)

used_keys=$(mktemp)
def_keys=$(mktemp)
trap 'rm -f "$used_keys" "$def_keys"' EXIT

# Keys referenced in code: l10n "x", l10nArgs "x", l10nFull "x"
grep -rhoE 'l10n(Args|Full)? "[a-z0-9-]+"' "${SRC_GLOBS[@]}" \
  | sed -E 's/.*"([a-z0-9-]+)"/\1/' | sort -u > "$used_keys"

# Keys defined in the .ftl: top-level "key = ..." (attributes are indented, so
# they don't match the start-of-line anchor).
grep -E '^[a-z][a-z0-9-]* =' "$FTL" \
  | sed -E 's/^([a-z0-9-]+) =.*/\1/' | sort -u > "$def_keys"

echo "Referenced keys: $(wc -l < "$used_keys") | Defined keys: $(wc -l < "$def_keys")"

missing=$(comm -23 "$used_keys" "$def_keys")
unused=$(comm -13 "$used_keys" "$def_keys")

status=0

if [ -n "$missing" ]; then
  echo
  echo "ERROR: keys used in code but NOT defined in $FTL (render as {key}):"
  echo "$missing" | sed 's/^/  - /'
  status=1
fi

if [ -n "$unused" ]; then
  echo
  echo "INFO: keys defined in $FTL but not referenced in toolkit code"
  echo "      (may be referenced indirectly, e.g. via {message-ref} in the .ftl):"
  echo "$unused" | sed 's/^/  - /'
fi

if [ "$status" -eq 0 ] && [ -z "$missing" ]; then
  echo
  echo "OK: all referenced keys are defined."
fi

exit "$status"
