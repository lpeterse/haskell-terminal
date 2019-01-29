.PHONY: install test clean doc terminal-ev terminal-out terminal-printer terminal-screen

install:
	stack install

test:
	stack test --ta "-t 3 -j1" --coverage

clean:
	stack clean

doc:
	stack haddock

terminal-ev: install
	~/.local/bin/terminal-ev

terminal-out: install
	~/.local/bin/terminal-out

terminal-printer: install
	~/.local/bin/terminal-printer

terminal-screen: install
	~/.local/bin/terminal-screen
