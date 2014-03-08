title = Sacrificed Sons
package = sacrificedsons
cover-name = octavarium
number = 7
jammit-trim = 46.526
album-trim = 45.914
fadein = 6.866
jammit-fadein = $(fadein)
album-fadein  = $(fadein)

clean:
	rm -rf gen

# MIDI

include ../../make/midi-one-tempo.mk

# Audio parts

include ../../make/jammit-drums.mk
include ../../make/album-drums.mk
include ../../make/countin.mk

# Audio merge

gen/%/audio.ogg: gen/%/drums.wav gen/%/song-countin.wav
	sox --combine merge $+ $@

# Metadata

include ../../make/album/$(cover-name).mk
include ../../make/dta-drums.mk

# Rock Band 3

include ../../make/rb3.mk
