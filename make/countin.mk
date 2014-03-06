gen/%/countin.wav: gen/%/notes.mid ../../sound/hihat-foot.wav
	../../scripts/countin $+ $@

gen/%/song-countin.wav: gen/%/song.wav gen/%/countin.wav
	sox --combine mix -v 1 $+ $@
