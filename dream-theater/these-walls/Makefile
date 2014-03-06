title = These Walls
artist = Dream Theater
package = thesewalls
cover-name = octavarium
jammit-trim = 11.711
album-trim = 19.991

clean:
	rm -rf gen

# MIDI

include ../../make/midi-one-tempo.mk

# Audio parts

include ../../make/jammit-drums.mk
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
