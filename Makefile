.PHONY: fmt \
		build \
		test

install:
	cabal install exe:efg

fmt:
	treefmt

build:
	cabal build all

test:
	cabal test all