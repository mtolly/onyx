#!/usr/bin/env fish

# batch script for working on and keeping track of rocksmith charts

rm -rf rsproj
mkdir rsproj

function build
  onyx build $argv[1] --target rs --to rsproj/(basename $argv[1])
end

# released:

# build songs/dream-theater/learning-to-live
# build songs/soil-and-pimp-sessions/summer-goddess
# build songs-others/bloodline/the-violation
# build songs-lik/other/a-crimson-rose-and-a-gin-tonic
# build songs/nova-collective/ripped-apart-and-reassembled # still need to do guitar phrases
# build songs-grinnz/dream-theater/stream-of-consciousness
# build songs/sungazer/sequence-start
# build songs/insaneintherainmusic/dark-world
# build songs-others/gigakoops/sun-of-pearl
# build songs/dream-theater/under-a-glass-moon
# build songs-hmx/tbrb/here-comes-the-sun
# build songs/opeth/windowpane

# maybe next release:

# build songs/dream-theater/a-rite-of-passage # needs 4-string, also needs tone
# build songs/toe/last-night
# build songs/billy-cobham/quadrant-4
# build songs/soil-and-pimp-sessions/scoop-out
# build songs/hiromi-uehara/spark # needs 4-string

# maybe next release but needs phrases:

# build songs/jethro-tull/thick-as-a-brick-pt-1
# build songs/jethro-tull/thick-as-a-brick-pt-2
# build songs-others/dragontaser/the-least-333-seconds
# build songs-others/bloodline/goliath # need heavy drive tone
# build songs/game-soundtracks/kagekiyo # need drive tone
# build songs-hmx/coheed-and-cambria/guns-of-summer # need synth/drive tone

# phrases done:

# build songs/dream-theater/repentance # needs 4-string
# build songs/dream-theater/forsaken # needs 4-string
# build songs/dream-theater/blind-faith # needs 4-string
# build songs-others/boo/puzzle-box # needs 4-string
# build songs-others/boo/head-hunter
# build songs-rbn/an-endless-sporadic/anything
# build songs/mathieu-fiset/chicks-pain
# build songs-others/bloodline/od # needs 4-string
# build songs/dream-theater/6-00 # needs 4-string
# build songs-others/moochalacho/camel-dancefloor # needs 4-string
# build songs-hmx/megadeth/hangar-18 # can't put on CF
# build songs/john-myung/solar-groove
# build songs/jizue/marten # needs 4-string
# build songs-others/guitar-hero/my-apocalypse
# build songs-others/guitar-hero/suicide-and-redemption-j-h
# build songs-others/bloodline/fish-amir-bresler-playthrough
# build songs-hmx/rb1/cant-let-go

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
# build songs-hmx/slipknot/pulse-of-the-maggots
# build songs-others/moochalacho/musette-maximum # needs 4-string
# build songs-grinnz/dream-theater/the-ytse-jam
# build songs-others/xane60/still-loving-you
# build songs-others/mazegeek999/the-glass-prison
# build songs-others/mazegeek999/fatal-tragedy
# build songs/dream-theater/the-mirror
# build songs/dream-theater/in-the-presence-of-enemies-pt-2
# build songs/dream-theater/the-holiday-spirit-carries-on
# build songs-grinnz/dream-theater/as-i-am
# build songs-grinnz/dream-theater/a-nightmare-to-remember
# build songs/lady-antebellum/just-a-kiss
# build songs-others/bloodline/spanish-bay
# build songs/dream-theater/the-great-debate
# build songs/dream-theater/lifting-shadows-off-a-dream
# build songs/dream-theater/lie
# build songs-hmx/dream-theater/on-the-backs-of-angels # can't put on CF
# build songs-others/xane60/fathom-infinite-depth
# build songs/dream-theater/anna-lee
# build songs/dream-theater/trial-of-tears
# build songs-hmx/billy-joel/piano-man
# build songs-others/bloodline/sure-shot
# build songs/trioscapes/stab-wounds
# build songs-hmx/the-black-dahlia-murder/what-a-horrible-night-to-have-a-curse
# build songs-hmx/whitechapel/this-is-exile
# build songs-rbn/the-dillinger-escape-plan/widower
# build songs-rbn/cynic/the-space-for-this
# build songs-others/bloodline/circus
# build songs/billy-cobham/stratus
# build songs/tormented-brutality/dominate
# build songs-hmx/metallica/ride-the-lightning
# build songs-rbn/anamanaguchi/airbrushed
# build songs-rbn/amberian-dawn/dreamchaser
# build songs-rbn/mike-orlando/burn
# build songs-rbn/prototype/the-way-it-ends
# build songs-others/boo/immediate-results
# build songs-others/bloodline/spark
# build songs/gyari/akane-chan
# build songs-hmx/trivium/in-waves
# build songs-others/inventor211/the-sin-and-the-sentence
# build songs/trivium/of-all-these-yesterdays
# build songs-others/guitar-hero/we-three-kings
# build songs-others/guitar-hero/that-was-just-your-life
# build songs/hiromi-uehara/caravan
# build songs-others/guitar-hero/dyers-eve
# build songs/metallica/blackened
# build songs/metallica/fight-fire-with-fire
# build songs/synovial/desert-hours
# build songs-others/cyclopsdragon/good-neighbor
# build songs/john-petrucci/jaws-of-life
# build songs-others/cyclopsdragon/when-i-lost-my-bet # this needs fixes
# build songs-hmx/slipknot/sulfur
# build songs/metallica/fuel
# build songs/metallica/one
# build songs-hmx/rb3/before-i-forget
# build songs-others/guitar-hero/all-nightmare-long
build songs/avenged-sevenfold/the-stage
build songs-rbn/benighted/let-the-blood-spill-between-my-broken-teeth

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
