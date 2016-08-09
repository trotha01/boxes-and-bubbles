MAKE=elm-make

all: example player eat transform duplicate
.PHONY: all

example:
	$(MAKE) example/Example.elm --output example/index.html
player: player/Player.elm
	$(MAKE) player/Player.elm --output player/index.html
eat: eat/Eat.elm
	$(MAKE) eat/Eat.elm --output eat/index.html
transform: transform/Transform.elm
	$(MAKE) transform/Transform.elm --output transform/index.html
duplicate: duplicate/Duplicate.elm
	$(MAKE) duplicate/Duplicate.elm --output duplicate/index.html
