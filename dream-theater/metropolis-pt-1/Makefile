title = Metropolis Pt. 1
artist = Dream Theater
package = metropolispt1
cover-name = images-and-words
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

gen/%/songs.dta: gen/%/notes.mid songs.dta
	mkdir -p $(@D)
	../../scripts/dtafill -p $(package) -m $+ $@

# Rock Band 3

include ../../make/rb3.mk
