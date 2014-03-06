title = Pull Me Under
artist = Dream Theater
package = pullmeunder
cover-name = images-and-words
jammit-pad = 2.314
hit-pad = 4.340
album-pad = 4.317

clean:
	rm -rf gen
	rm -f ../../make/hit-drums.mk

# MIDI

include ../../make/midi-two-tempos.mk

gen/hit/%/notes.mid: gen/jammit/%/notes.mid
	mkdir -p $(@D)
	cp $< $@

# Audio parts

include ../../make/jammit-drums.mk
include ../../make/album-drums.mk
include ../../make/hit-drums.mk
include ../../make/countin.mk

../../make/hit-drums.mk: ../../make/album-drums.mk
	sed 's/album/hit/g' $< > $@

# Audio merge

gen/%/audio.ogg: gen/%/drums.wav gen/%/song-countin.wav
	sox --combine merge $+ $@

# Metadata

gen/%/songs.dta: gen/%/notes.mid songs.dta
	mkdir -p $(@D)
	../../scripts/dtafill -p $(package) -m $+ $@

# Rock Band 3

include ../../make/rb3.mk
