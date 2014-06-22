# Stereo drums and bass.

config = drums-bass

SHELL = /bin/bash

clean:
	rm -rf gen

### JAMMIT AUDIO
# Requires the Jammit drums and/or bass package.

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
	if $(jammit-cmd) -c -y d; then                        \
		if $(jammit-cmd) -c -y b; then                      \
			$(jammit-cmd) -y D -n b -a $@;                    \
		else                                                \
			$(jammit-cmd) -y D -a $@;                         \
		fi;                                                 \
	else                                                  \
		if $(jammit-cmd) -c -y b; then                      \
			$(jammit-cmd) -y B -a $@;                         \
		else                                                \
			echo "Couldn't find Jammit drums or bass track."; \
			false;                                            \
		fi;                                                 \
	fi

gen/jammit/%p/drums-untimed.wav:
	mkdir -p $(@D)
	if $(jammit-cmd) -c -y d; then                    \
		$(jammit-cmd) -y d -a $@;                       \
	else                                              \
		sox -n -b 16 $@ rate 44100 channels 2 trim 0 1; \
	fi

gen/jammit/%p/bass-untimed.wav:
	mkdir -p $(@D)
	if $(jammit-cmd) -c -y b; then                    \
		$(jammit-cmd) -y b -a $@;                       \
	else                                              \
		sox -n -b 16 $@ rate 44100 channels 2 trim 0 1; \
	fi

gen/jammit/%.wav: gen/jammit/%-untimed.wav
	sox $< $@ trim $(jammit-trim) fade t $(jammit-fadein) pad $(jammit-pad)

### RB3 OGG
# 6 tracks: drums L, drums R, bass L, bass R, backing L, backing R
# We add a 7th track of silence because oggenc assumes a 6-channel vorbis file
# is in 5.1, so the 6th channel (LFE) gets screwed up.

include ../../make/countin.mk

gen/fake-lfe.wav:
	mkdir -p $(@D)
	sox -n -b 16 $@ rate 44100 channels 1 trim 0 1

gen/%p/audio.ogg: gen/%p/drums.wav gen/%p/bass.wav gen/%p/song-countin.wav gen/fake-lfe.wav
	sox --combine merge $+ $@

### SHARED RULES
include ../../make/shared.mk
