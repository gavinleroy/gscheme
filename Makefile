
.PHONY: all
all: run

.PHONY: run
run:
	@dune exe ./gscheme.exe

.PHONY: build
build:
	@dune build gscheme.exe

.PHONY: test
test:
	@dune runtest

.PHONY: clean
clean:
	@rm -rf _build
