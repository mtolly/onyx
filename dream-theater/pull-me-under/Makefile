title = Pull Me Under
package = pullmeunder
cover-name = images-and-words
number = 1
jammit-pad = 2.314
hit-pad = 4.340
album-pad = 4.317

include ../../make/config/drums.mk

gen/make/hit-single.mk: ../../make/album-single.mk
	mkdir -p $(@D)
	sed 's/album/hit/g' $< > $@

include gen/make/hit-single.mk
