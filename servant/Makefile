PROJECT_ROOT ?= $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
HASKELL_DIRS := $(addprefix $(PROJECT_ROOT)/,app src)
HASKELL_FILES := $(shell find $(HASKELL_DIRS) -name '*.hs')

.PHONY: watch
watch:
	ghcid \
		--command='cabal v2-repl' \
		--no-height-limit \
		--reverse-errors \
		--clear \
		--lint

.PHONY: build
build:
	cabal v2-build

.PHONY: format
format:
	ormolu --mode inplace $(HASKELL_FILES)

.PHONY: run-server
run-server:
	cabal v2-run servant-server-cookbook

.PHONY: run-client
run-client:
	cabal v2-run servant-client-cookbook

.PHONY: run-int-server
run-int-server:
	cabal v2-run servant-int-server-cookbook

.PHONY: run-hoisted-client
run-hoisted-client:
	cabal v2-run servant-hoisted-client-cookbook

.PHONY: run-stream-server
run-stream-server:
	cabal v2-run servant-stream-server-cookbook

.PHONY: run-stream-client
run-stream-client:
	cabal v2-run servant-stream-client-cookbook

.PHONY: run-javascript-server
run-javascript-server:
	cabal v2-run servant-javascript-server-cookbook

.PHONY: run-basic-auth-server
run-basic-auth-server:
	cabal v2-run servant-basic-auth-server-cookbook

.PHONY: run-gen-auth-server
run-gen-auth-server:
	cabal v2-run servant-gen-auth-server-cookbook

.PHONY: run-structuring-apis
run-structuring-apis:
	cabal v2-run servant-structuring-apis

.PHONY: run-generic-server
run-generic-server:
	cabal v2-run servant-generic-server -- run

.PHONY: run-generic-server-custom
run-generic-server-custom:
	cabal v2-run servant-generic-server -- run-custom-monad

.PHONY: run-generic-client
run-generic-client:
	cabal v2-run servant-generic-client

.PHONY: run-https-server
run-https-server:
	cabal v2-run servant-https-server

.PHONY: serve-docs
serve-docs:
	cabal v2-run serve-docs

.PHONY: clean
clean:
	rm -rf $(PROJECT_ROOT)/dist
	rm -rf $(PROJECT_ROOT)/dist-newstyle
	rm -f  $(PROJECT_ROOT)/.ghc.environment.*
