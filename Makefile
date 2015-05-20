all:
	elm make src/Main.elm --output=public/elm.js

deps:
	elm package install --yes
	npm install
