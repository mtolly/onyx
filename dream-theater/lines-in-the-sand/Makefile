title = Lines in the Sand
package = linesinthesand
cover-name = falling-into-infinity
number = 7
jammit-trim = 2.222
jammit-fadein = 4.180
jammit-pad = 2.849
album-fadein = 4.261
album-pad = 2.768

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

# Magma

include ../../make/magma-drums.mk
