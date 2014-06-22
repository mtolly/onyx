# Just stereo drums.

config = drums

SHELL = /bin/bash

clean:
	rm -rf gen

### JAMMIT AUDIO
# Requires the Jammit drums package.

# Required variables:
jammit-title  ?= $(title)
jammit-artist ?= $(artist)

# Optional variables:
jammit-trim   ?= 0
jammit-fadein ?= 0
jammit-pad    ?= 0

jammit-cmd = jammittools -T "$(jammit-title)" -R "$(jammit-artist)"

gen/jammit/%p/song-untimed.wav:
	mkdir -p $(@D)
	$(jammit-cmd) -y D -a $@

gen/jammit/%p/drums-untimed.wav:
	mkdir -p $(@D)
	$(jammit-cmd) -y d -a $@

gen/jammit/%.wav: gen/jammit/%-untimed.wav
	sox $< $@ trim $(jammit-trim) fade t $(jammit-fadein) pad $(jammit-pad)

### RB3 OGG
# 4 tracks: drums L, drums R, backing L, backing R

include ../../make/countin.mk

gen/%p/audio.ogg: gen/%p/drums.wav gen/%p/song-countin.wav
	sox --combine merge $+ $@

### SHARED RULES
include ../../make/shared.mk
