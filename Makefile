all:
	elm make src/Main.elm --output=public/elm.js

install:
	elm-package install
	npm install
