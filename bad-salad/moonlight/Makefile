title = Moonlight
package = moonlight
cover-name = puzzled
number = 2
pledgemusic-trim = 0.326
pledgemusic-multiply = 0.6
album-pad = 3.631

include ../../make/config/drums.mk

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
