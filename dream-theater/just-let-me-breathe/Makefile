title = Just Let Me Breathe
package = justletmebreathe
cover-name = falling-into-infinity
number = 9
jammit-pad = 1.901
album-pad = 3.641

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
