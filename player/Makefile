.PHONY: build minify

build:
	pulp build --to www/app.js
	cp www/app.js www/app.min.js

minify: build
	closure-compiler www/app.js > www/app.min.js
