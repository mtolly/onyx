# Shared by fluid-dump and fluid-probe: build the patched tree, and make sure
# the built binary can actually start. Sourced, not executed.
#
# $ROOT must already be set to the haskell directory.

# The app looks for its resources next to the executable, which a plain
# `stack build` does not arrange. Without them onyx dies during startup, and
# the caller is left screenshotting an empty desktop or dumping nothing at all.
fluid_resources() {
  local bin
  bin=$(cd "$ROOT" && ./stack-local path --local-install-root)/bin
  if [ ! -e "$bin/onyx-resources" ]; then
    ln -sfn "$ROOT/resources" "$bin/onyx-resources"
  fi
}

# A failed build must stop us: otherwise the app that gets run afterwards is a
# stale binary from an earlier build, and every result after that is a quiet lie.
fluid_build() {
  local log
  log=$(mktemp)
  if ! (cd "$ROOT" && ./stack-local build) >"$log" 2>&1; then
    echo "build failed:" >&2
    grep -E "error" -A 6 "$log" | head -40 >&2
    rm -f "$log"
    exit 1
  fi
  rm -f "$log"
  fluid_resources
}
