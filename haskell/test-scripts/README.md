Standalone test scripts, compiled against the built onyx-lib.

Build and run (after `./stack-local build onyx-lib`):

    stack exec ghc -- MiloRoundtrip.hs -o milo-roundtrip -O1
    ./milo-roundtrip ../rb3-iso ../rb3-dlc

(Use `stack exec` directly rather than `./stack-local exec`, since the
wrapper's extra flags confuse ghc; set LD_LIBRARY_PATH to
dependencies/root/lib if needed.)

MiloRoundtrip.hs: for every .milo_* file under the given directories,
decompresses it, parses the dir structure, re-serializes, and verifies the
result is byte-identical to the decompressed original.
