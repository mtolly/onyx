title = Afterlife
package = dtafterlife
# package name "afterlife" doesn't work
# probably conflicts with RB3 disc files relating to A7X afterlife
cover-name = when-dream-and-day-unite
number = 6
album-pad = 2.704
next-cutoff = 10.383
album-fadeout = 1.357

clean:
	rm -rf gen

# MIDI

include ../../make/midi-one-tempo.mk

# Audio parts

include ../../make/album-double.mk
include ../../make/countin.mk

# Audio merge

gen/%/audio.ogg: gen/%/drums.wav gen/%/song-countin.wav
	sox --combine merge $+ $@

# Metadata

include ../../make/album/$(cover-name).mk
include ../../make/dta-drums.mk

# Rock Band 3

include ../../make/rb3.mk
