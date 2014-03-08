title = A Fortune in Lies
package = afortuneinlies
cover-name = when-dream-and-day-unite
number = 1
album-pad = 1.724

clean:
	rm -rf gen

# MIDI

include ../../make/midi-one-tempo.mk

# Audio parts

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
