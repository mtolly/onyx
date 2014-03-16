gen/%p/countin.wav: gen/%p/notes.mid ../../sound/hihat-foot.wav
	../../scripts/countin $+ $@

gen/%p/song-countin.wav: gen/%p/song.wav gen/%p/countin.wav
	sox --combine mix -v 1 $+ $@
