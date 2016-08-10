MAKE=elm-make

all: example/index.html player/index.html eat/index.html transform/index.html duplicate/index.html
.PHONY: all

example/index.html: example/Example.elm
	elm-format example/*.elm --yes
	$(MAKE) example/Example.elm --output example/index.html
player/index.html: player/Player.elm
	elm-format player/*.elm --yes
	$(MAKE) player/Player.elm --output player/index.html
eat/index.html: eat/Eat.elm
	elm-format eat/*.elm --yes
	$(MAKE) eat/Eat.elm --output eat/index.html
transform/index.html: transform/Transform.elm
	elm-format transform/*.elm --yes
	$(MAKE) transform/Transform.elm --output transform/index.html
duplicate/index.html: duplicate/*.elm
	elm-format duplicate/*.elm --yes
	$(MAKE) duplicate/Duplicate.elm --output duplicate/index.html
