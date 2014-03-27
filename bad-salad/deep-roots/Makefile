title = Deep Roots
package = deeproots
cover-name = puzzled
number = 3
pledgemusic-multiply = 0.65
album-pad = 3.532
stem-prefix = BadSalad-Puzzled-3-DeepRoots-Stem-

include ../../make/config/drums.mk

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
