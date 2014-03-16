title = Metropolis Pt. 1
package = metropolispt1
cover-name = images-and-words
number = 5
album-trim = 0.379

clean:
	rm -rf gen

# MIDI

include ../../make/midi-two-tempos.mk

# Audio parts

include ../../make/jammit-drums.mk
include ../../make/album-drums.mk

# Audio merge

gen/%/audio.ogg: gen/%/drums.wav gen/%/song.wav
	sox --combine merge $+ $@

# Metadata

include ../../make/album/$(cover-name).mk
include ../../make/dta-drums.mk

# Rock Band 3

include ../../make/rb3.mk

# Magma

include ../../make/magma-drums.mk
