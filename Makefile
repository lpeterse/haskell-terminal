.PHONY: install test

install:
	stack install

test:
	stack test --ta "-t 3 -j1" --coverage
