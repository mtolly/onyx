### ALBUM AUDIO

album-config ?= single
include ../../make/album-$(album-config).mk

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
