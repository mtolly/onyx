gen/%/1p/notes.mid: notes.mid
	mkdir -p $(@D)
	if [ -a tempo-$*.mid ]; then \
		../../scripts/replace-tempos $< tempo-$*.mid $@; \
	else \
		cp $< $@; \
		../../scripts/fix-resolution $@; \
	fi

gen/%/2p/notes.mid: gen/%/1p/notes.mid
	mkdir -p $(@D)
	../../scripts/2x-bass-pedal $< $@