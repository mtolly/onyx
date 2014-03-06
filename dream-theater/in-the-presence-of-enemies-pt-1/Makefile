title = In the Presence of Enemies, Pt. 1
artist = Dream Theater
package = itpoept1
cover-name = systematic-chaos
album-pad = 4.637
album-take = 8:57.302
album-fadeout = 2.75

clean:
	rm -rf gen

# MIDI

include ../../make/midi-one-tempo.mk

# Audio parts

gen/album/%/song.wav: audio-album.*
	mkdir -p $(@D)
	../../scripts/audioconvert $< $@ \
		pad $(album-pad) \
		fade t 0 $(album-take) $(album-fadeout) \
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
