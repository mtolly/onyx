gen/%/1p/notes.mid: notes.mid
	mkdir -p $(@D)
	cp $< $@
	../../scripts/fix-resolution $@

gen/%/2p/notes.mid: gen/%/1p/notes.mid
	mkdir -p $(@D)
	../../scripts/2x-bass-pedal $< $@
