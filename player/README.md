An HTML5 Canvas app for viewing and playing back Rock Band songs,
written in [PureScript](http://www.purescript.org/).

Currently built with `purs 0.12.0`.

To build:

```bash
[sudo] npm install -g purescript pulp psc-package
psc-package build
make # or make minify to also run closure compiler
```

This generates `app.min.js` in the `www/` directory.
The `www/` contents are then stored inside the `onyx` executable
(via [`file-embed`](https://hackage.haskell.org/package/file-embed)).

`onyx` performs the initial work of processing the audio and MIDI into a convenient form,
and then spits out a self-contained folder for a song's preview app.
This folder contains:

* everything from `www/` (including `app.min.js`)
* `preview-audio.ogg` and `preview-audio.mp3`, the rendered audio
* `song.js`, the actual note data
