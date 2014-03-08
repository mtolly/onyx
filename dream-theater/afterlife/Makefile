title = Afterlife
package = dtafterlife
# package name "afterlife" doesn't work
# probably conflicts with RB3 disc files relating to A7X afterlife
cover-name = when-dream-and-day-unite
number = 6
this-pad = 2.704
next-take = 10.383
next-fadeout = 1.357

clean:
	rm -rf gen

# MIDI

include ../../make/midi-one-tempo.mk

# Audio parts

gen/album/%/this.wav: audio-this.*
	mkdir -p $(@D)
	../../scripts/audioconvert $< $@ pad $(this-pad) rate 44100 channels 2

gen/album/%/next.wav: audio-next.*
	mkdir -p $(@D)
	../../scripts/audioconvert $< $@ \
		fade t 0 $(next-take) $(next-fadeout) \
		rate 44100 \
		channels 2

gen/album/%/song.wav: gen/album/%/this.wav gen/album/%/next.wav
	sox --combine concatenate $+ $@

gen/album/%/drums.wav: gen/album/%/song.wav
	sox $< $@ pad 1 trim 0 1

include ../../make/countin.mk

# Audio merge

gen/%/audio.ogg: gen/%/drums.wav gen/%/song-countin.wav
	sox --combine merge $+ $@

# Metadata

include ../../make/album/$(cover-name).mk
include ../../make/dta-drums.mk

# Rock Band 3

include ../../make/rb3.mk
