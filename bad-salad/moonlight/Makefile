title = Moonlight
package = moonlight
cover-name = puzzled
number = 2
pledgemusic-trim = 0.326
pledgemusic-multiply = 0.6
album-pad = 3.631
stem-prefix = BadSalad-Puzzled-2-Moonlight-Stem-

include ../../make/config/drums.mk

gen/pledgemusic/%/drums-untimed.wav: $(stem-prefix)Drums.*
	mkdir -p $(@D)
	sox -v $(pledgemusic-multiply) $< $@

gen/pledgemusic/%/song-untimed.wav: \
		$(stem-prefix)Bass.* \
		$(stem-prefix)Guitars.* \
		$(stem-prefix)Keyboards.* \
		$(stem-prefix)Vocals.*
	mkdir -p $(@D)
	sox --combine mix $(addprefix -v $(pledgemusic-multiply) ,$+) $@

gen/pledgemusic/%.wav: gen/pledgemusic/%-untimed.wav
	sox $< $@ trim $(pledgemusic-trim)
