title = Moonlight
artist = Bad Salad
package = moonlight
cover-name = puzzled
pledgemusic-trim = 0.326
pledgemusic-multiply = 0.6
album-pad = 3.631

clean:
	rm -rf gen

# MIDI

include ../../make/midi-one-tempo.mk

# Audio parts

gen/pledgemusic/%/drums-untimed.wav: BadSalad-Puzzled-2-Moonlight-Stem-Drums.*
	mkdir -p $(@D)
	sox -v $(pledgemusic-multiply) $< $@

gen/pledgemusic/%/song-untimed.wav: \
		BadSalad-Puzzled-2-Moonlight-Stem-Bass.* \
		BadSalad-Puzzled-2-Moonlight-Stem-Guitars.* \
		BadSalad-Puzzled-2-Moonlight-Stem-Keyboards.* \
		BadSalad-Puzzled-2-Moonlight-Stem-Vocals.*
	mkdir -p $(@D)
	sox --combine mix $(addprefix -v $(pledgemusic-multiply) ,$+) $@

gen/pledgemusic/%.wav: gen/pledgemusic/%-untimed.wav
	sox $< $@ trim $(pledgemusic-trim)

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
