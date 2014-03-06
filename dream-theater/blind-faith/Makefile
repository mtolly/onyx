title = Blind Faith
artist = Dream Theater
package = blindfaith
cover-name = six-degrees-of-inner-turbulence
album-1-trim = 10.991
album-2-fadein = 5.673
album-3-cutoff = 9:58.000
# sneaking under that Harmonix cutoff
album-4-fadeout = 5.200

clean:
	rm -rf gen

# MIDI

include ../../make/midi-one-tempo.mk

# Audio parts

gen/album/%/song.wav: audio-album.*
	mkdir -p $(@D)
	../../scripts/audioconvert $< $@ \
		trim $(album-1-trim) \
		fade t $(album-2-fadein) $(album-3-cutoff) $(album-4-fadeout) \
		rate 44100 \
		channels 2

gen/album/%/drums.wav: gen/album/%/song.wav
	sox $< $@ pad 1 trim 0 1

include ../../make/countin.mk

# Audio merge

gen/%/audio.ogg: gen/%/drums.wav gen/%/song-countin.wav
	sox --combine merge $+ $@

# Metadata

gen/%/songs.dta: gen/%/notes.mid songs.dta
	mkdir -p $(@D)
	../../scripts/dtafill -p $(package) -m $+ $@

# Rock Band 3

include ../../make/rb3.mk
