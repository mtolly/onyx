# required variables:
# title or jammit-title, artist or jammit-artist
jammit-title  ?= $(title)
jammit-artist ?= $(artist)

# optional variables:
jammit-trim   ?= 0
jammit-fadein ?= 0
jammit-pad    ?= 0

gen/jammit/%/song-untimed.wav:
	mkdir -p $(@D)
	jammittools -T "$(jammit-title)" -R "$(jammit-artist)" -y D -a $@

gen/jammit/%/drums-untimed.wav:
	mkdir -p $(@D)
	jammittools -T "$(jammit-title)" -R "$(jammit-artist)" -y d -a $@

gen/jammit/%.wav: gen/jammit/%-untimed.wav
	sox $< $@ trim $(jammit-trim) fade t $(jammit-fadein) pad $(jammit-pad)