title = Blind Faith
package = blindfaith
cover-name = six-degrees-of-inner-turbulence
number = 2
album-trim = 10.991
album-fadein = 5.673
album-cutoff = 9:58.000
# sneaking under that Harmonix cutoff
album-fadeout = 5.200

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
