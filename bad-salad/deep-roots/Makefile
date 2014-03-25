title = Deep Roots
package = deeproots
cover-name = puzzled
number = 3
pledgemusic-multiply = 0.65
album-pad = 3.532
stem-prefix = BadSalad-Puzzled-3-DeepRoots-Stem-

clean:
	rm -rf gen

# MIDI

include ../../make/midi-one-tempo.mk

# Audio parts

gen/pledgemusic/%/drums.wav: $(stem-prefix)Drums.*
	mkdir -p $(@D)
	sox -v $(pledgemusic-multiply) $< $@

gen/pledgemusic/%/song.wav: \
		$(stem-prefix)Bass.* \
		$(stem-prefix)Guitars.* \
		$(stem-prefix)Keyboards.* \
		$(stem-prefix)Vocals.*
	mkdir -p $(@D)
	sox --combine mix $(addprefix -v $(pledgemusic-multiply) ,$+) $@

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

# Magma

include ../../make/magma-drums.mk
