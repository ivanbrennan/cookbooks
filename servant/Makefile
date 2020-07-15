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

.PHONY: run-overview-server
run-overview-server:
	cabal v2-run servant-overview-server

.PHONY: run-overview-migrate
run-overview-migrate:
	cabal v2-run servant-overview-server -- migrate

.PHONY: run-sqlite-app
run-sqlite-app:
	cabal v2-run servant-sqlite-app

.PHONY: run-postgresql-connection-pool
run-postgresql-connection-pool:
	cabal v2-run servant-postgresql-connection-pool

.PHONY: run-stm-server
run-stm-server:
	cabal v2-run servant-stm-server

.PHONY: run-client-free-server
run-client-free-server:
	cabal v2-run servant-client-free -- server

.PHONY: run-client-free-client
run-client-free-client:
	cabal v2-run servant-client-free -- client

.PHONY: run-another-basic-auth
run-another-basic-auth:
	cabal v2-run servant-another-basic-auth

.PHONY: run-basic-streaming-server
run-basic-streaming-server:
	cabal v2-run servant-basic-streaming server

.PHONY: run-basic-streaming-client
run-basic-streaming-client:
	cabal v2-run servant-basic-streaming client 10

.PHONY: serve-docs
serve-docs:
	cabal v2-run serve-docs

.PHONY: clean
clean:
	rm -rf $(PROJECT_ROOT)/dist
	rm -rf $(PROJECT_ROOT)/dist-newstyle
	rm -f  $(PROJECT_ROOT)/.ghc.environment.*
