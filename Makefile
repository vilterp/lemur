all:
	elm make src/Main.elm --output=public/elm.js

deps:
	elm package install --yes
	npm install

loc:
	find src -regex ".*elm" | xargs wc -l

diagramsloc:
	find elm-diagrams/Diagrams -regex ".*elm" | xargs wc -l

