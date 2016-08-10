MAKE=elm-make

all: example/index.html player/index.html eat/index.html transform/index.html duplicate/index.html
.PHONY: all

example/index.html: example/Example.elm
	$(MAKE) example/Example.elm --output example/index.html
player/index.html: player/Player.elm
	$(MAKE) player/Player.elm --output player/index.html
eat/index.html: eat/Eat.elm
	$(MAKE) eat/Eat.elm --output eat/index.html
transform/index.html: transform/Transform.elm
	$(MAKE) transform/Transform.elm --output transform/index.html
duplicate/index.html: duplicate/*.elm
	$(MAKE) duplicate/Duplicate.elm --output duplicate/index.html
