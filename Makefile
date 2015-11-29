all:
	# https://github.com/elm-lang/elm-make/issues/33
	export LANG=en_US.UTF-8 && elm make --yes src/Main.elm --output=public/elm.js

deps:
	elm package install --yes
	git submodule init
	git submodule update
	npm install

clean:
	rm -rf elm-stuff
	rm public/elm.js

loc:
	find src -regex ".*elm" | xargs wc -l
	find elm-diagrams/Diagrams -regex ".*elm" | xargs wc -l

