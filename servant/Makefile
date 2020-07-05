PROJECT_ROOT ?= $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
HASKELL_DIRS := $(addprefix $(PROJECT_ROOT)/,app src)
HASKELL_FILES := $(shell find $(HASKELL_DIRS) -name '*.hs')

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

.PHONY: format
format:
	ormolu --mode inplace $(HASKELL_FILES)

.PHONY: run-server
run-server:
	cabal v2-run servant-server-cookbook

.PHONY: run-client
run-client:
	cabal v2-run servant-client-cookbook

.PHONY: clean
clean:
	rm -rf $(PROJECT_ROOT)/dist
	rm -rf $(PROJECT_ROOT)/dist-newstyle
	rm -f  $(PROJECT_ROOT)/.ghc.environment.*
