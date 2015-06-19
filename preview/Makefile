.PHONY: build minify

build:
	cabal configure --ghcjs
	cabal build
	cp dist/build/onyxpreview/onyxpreview.jsexe/{rts,lib,out,runmain}.js www/
	cat www/{prefix,rts,lib,out,suffix}.js > www/app.js
	cp www/app.js www/app.min.js

minify: build
	closure-compiler www/app.js > www/app.min.js
