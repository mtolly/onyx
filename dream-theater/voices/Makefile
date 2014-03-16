title = Voices
jammit-title = A Mind Beside Itself II. Voices
package = voices
cover-name = awake
number = 5
jammit-trim = 3.183
jammit-fadein = 0.477
jammit-pad = 2.877
album-fadein = 0.538
album-pad = 2.893

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
