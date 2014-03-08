title = Light Fuse and Get Away
package = lightfuseandgetaway
cover-name = when-dream-and-day-unite
number = 5
album-pad = 0.611

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
