.PHONY: install test

PREFIX ?= $(HOME)/.local
LISP := $(shell find . -name '*.lisp' -o -name '*.asd')

kiln: $(LISP)
	sh build.sh

test: kiln
	./kiln self-test

install: kiln
	mkdir -p $(PREFIX)/bin
	ln -s $$(pwd -P)/kiln $(PREFIX)/bin
