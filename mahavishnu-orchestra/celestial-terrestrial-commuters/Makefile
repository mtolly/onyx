title = Celestial Terrestrial Commuters
package = celestialcommuters
cover-name = birds-of-fire
number = 3
jammit-pad = 3.649
album-pad = 4.381

clean:
	rm -rf gen

# MIDI

include ../../make/midi-two-tempos.mk

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
