.PHONY: build minify

build:
	stack --stack-yaml=../stack-ghcjs.yaml build .
	cp .stack-work/dist/*/*/build/onyxpreview/onyxpreview.jsexe/{rts,lib,out,runmain}.js www/
	cat www/{prefix,rts,lib,out,suffix}.js > www/app.js
	cp www/app.js www/app.min.js

minify: build
	uglifyjs www/app.js -c -m > www/app.min.js
