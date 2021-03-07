#!/usr/bin/env fish

# batch script for working on and keeping track of rocksmith charts

rm -rf rsproj
mkdir rsproj

function build
  onyx shake $argv[1] gen/target/rs/cst
  and cp -R $argv[1]/gen/target/rs/cst rsproj/(basename $argv[1])
end

# phrases done:

# build songs/dream-theater/repentance # needs 4-string
# build songs/dream-theater/learning-to-live
# build songs/dream-theater/forsaken # needs 4-string
# build songs/dream-theater/blind-faith # needs 4-string
# build songs/soil-and-pimp-sessions/summer-goddess
# build songs/soil-and-pimp-sessions/scoop-out
# build songs/hiromi-uehara/spark # needs 4-string
# build songs-others/boo/puzzle-box # needs 4-string
# build songs-others/bloodline/the-violation
# build songs-lik/other/a-crimson-rose-and-a-gin-tonic
# build songs/nova-collective/ripped-apart-and-reassembled # still need to do guitar phrases
# build songs-grinnz/dream-theater/stream-of-consciousness
# build songs/sungazer/sequence-start
# build songs/insaneintherainmusic/dark-world
# build songs-others/boo/head-hunter
# build songs-others/gigakoops/sun-of-pearl
# build songs-rbn/an-endless-sporadic/anything
# build songs/dream-theater/under-a-glass-moon
# build songs/mathieu-fiset/chicks-pain
# build songs-others/bloodline/od # needs 4-string
# build songs-hmx/tbrb/here-comes-the-sun
# build songs/opeth/windowpane
# build songs/dream-theater/a-rite-of-passage # needs 4-string
# build songs/dream-theater/6-00 # needs 4-string

# phrases not done:

# build songs/dream-theater/bridges-in-the-sky # needs 4-string
# build songs/dream-theater/sacrificed-sons # needs 4-string
# build songs/dream-theater/voices # needs 4-string
# build songs/dream-theater/hells-kitchen # needs 4-string (and 5-string)
# build songs/nova-collective/state-of-flux # needs 4-string
# build songs/nova-collective/air # needs 4-string
# build songs/nova-collective/dancing-machines # needs 4-string
# build songs-others/boo/1985 # needs 4-string
# build songs-others/mazegeek999/beyond-this-life # needs 4-string (and 5-string)
# build songs/jethro-tull/thick-as-a-brick-pt-1
# build songs/trioscapes/blast-off # needs 4-string
# build songs-grinnz/between-the-buried-and-me/mirrors # needs 4-string
# build songs-grinnz/between-the-buried-and-me/obfuscation # needs 4-string
# build songs-grinnz/between-the-buried-and-me/disease-injury-madness # needs 4-string
# build songs-grinnz/between-the-buried-and-me/desert-of-song # needs 4-string
# build songs/dream-theater/another-day
# build songs/dream-theater/take-the-time # needs 4-string
# build songs/dream-theater/surrounded
# build songs/dream-theater/caught-in-a-web
# build songs/juliaplaysgroove/break-my-heart
# build songs/liquid-tension-experiment/paradigm-shift
# build songs-others/dragontaser/the-least-333-seconds
build songs-hmx/slipknot/pulse-of-the-maggots
build songs-others/bloodline/goliath

# phrases unknown:

# build songs-grinnz/dream-theater/this-dying-soul # needs 4-string
# build songs-grinnz/dream-theater/in-the-name-of-god # needs 4-string and 5-string
# build songs-others/boo/the-good-doctor # needs 4-string
# build songs-others/boo/veil # needs 4-string
# build songs-others/boo/messiah-complex # needs 4-string and 6-string
# build songs-grinnz/between-the-buried-and-me/foam-born # needs 4-string
# build songs-grinnz/between-the-buried-and-me/informal-gluttony # needs 4-string
# build songs-grinnz/between-the-buried-and-me/sun-of-nothing # needs 4-string
# build songs-grinnz/between-the-buried-and-me/ants-of-the-sky # needs 4-string
# build songs-grinnz/between-the-buried-and-me/prequel-to-the-sequel # needs 4-string
# build songs-grinnz/between-the-buried-and-me/viridian-white-walls # needs 4-string
# build songs/dream-theater/erotomania # needs 4-string and 5-string
# build songs/dream-theater/enigma-machine # needs 4-string and 5-string
# build songs/dream-theater/outcry # needs 4-string and 5-string
# build songs/dream-theater/breaking-all-illusions # needs 4-string and 5-string
# build songs/dream-theater/in-the-presence-of-enemies-pt-1 # needs 4-string
# build songs-hmx/dream-theater/constant-motion # needs 4-string
# build songs/dream-theater/the-dark-eternal-night # needs 4-string
# build songs/dream-theater/prophets-of-war
# build songs/soil-and-pimp-sessions/sabotage
# build songs/soil-and-pimp-sessions/the-black-widow-blues
# build songs-lik/jazz-pack-1/one-for-antonio
# build songs/louis-de-mieulle/the-taste-of-filth # needs 4-string
# build songs/movie-soundtracks/caravan
# build songs-others/cyclopsdragon/ridiculosous
