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

### ALBUM AUDIO

album-config ?= single
include ../../make/album-$(album-config).mk

### RB3 OGG
# 6 tracks: drums L, drums R, bass L, bass R, backing L, backing R

include ../../make/countin.mk

gen/%p/audio.ogg: gen/%p/drums.wav gen/%p/bass.wav gen/%p/song-countin.wav
	sox --combine merge $+ $@

### MIDI

include ../../make/midi.mk

### METADATA

include ../../make/album/$(cover-name).mk
include ../../make/dta/$(config).mk

### COMPILE

# Rock Band 3
include ../../make/rb3.mk

# Magma
include ../../make/magma/$(config).mk

# Frets on Fire
include ../../make/fof/$(config).mk
