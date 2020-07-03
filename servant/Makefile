PROJECT_ROOT ?= $(abspath $(dir $(lastword $(MAKEFILE_LIST))))

.PHONY: build
build:
	cabal v2-build

.PHONY: watch
watch:
	ghcid \
		--command='cabal v2-repl' \
		--no-height-limit \
		--reverse-errors \
		--clear \
		--lint

.PHONY: run
run:
	cabal v2-run

.PHONY: clean
clean:
	rm -rf $(PROJECT_ROOT)/dist
	rm -rf $(PROJECT_ROOT)/dist-newstyle
	rm -f  $(PROJECT_ROOT)/.ghc.environment.*
