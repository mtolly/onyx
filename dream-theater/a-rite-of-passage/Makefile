title = A Rite of Passage
package = ariteofpassage
cover-name = black-clouds-silver-linings
number = 2
album-pad = 4.656

clean:
	rm -rf gen

# MIDI

include ../../make/midi-one-tempo.mk

# Audio parts

# TODO: BCSL deluxe edition stems, will require heavy remixing
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

# Magma

include ../../make/magma-drums.mk
