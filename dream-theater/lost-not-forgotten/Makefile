title = Lost Not Forgotten
artist = Dream Theater
package = lostnotforgotten
cover-name = a-dramatic-turn-of-events
album-trim = 7.088
album-fadein = 3.088

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

gen/%/songs.dta: gen/%/notes.mid songs.dta
	mkdir -p $(@D)
	../../scripts/dtafill -p $(package) -m $+ $@

# Rock Band 3

include ../../make/rb3.mk
