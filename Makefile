.PHONY: help install watch compile-contracts build-dapp deploy-contracts test-dapp build-server
.DEFAULT_GOAL := help

export

# see https://stackoverflow.com/a/26936855/1798418
PATH  := node_modules/.bin:$(PATH)
SHELL := /bin/bash

NODE_URL ?= http://localhost:8545
ABIS_DIR ?= build

# default is cliquebait
NODE_URL = http://localhost:8545
SERVER_PORT = 9000
SERVER_BASE_HOST ?= localhost
SERVER_BASE_URL ?= //$(SERVER_BASE_HOST)
API_BASE_URL ?= $(SERVER_BASE_URL):$(SERVER_PORT)/

PG_BASE_DATABASE ?= postgres
PGDATABASE ?= signal_market
PGHOST ?= localhost
PGPORT ?= 5555
PGPASSWORD ?= password
PGUSER ?= postgres

GRAPHQL_PORT ?= 3003
GRAPHQL_API ?= 3003
GRAPHQL_PATH ?= /
GRAPHQL_PG_SCHEMAS ?= public
GRAPHQL_SIMPLE_AUTH_TOKEN ?= super_secret_token
GRAPHQL_API_URL ?= http://localhost:3003/graphql

PSQL ?= docker-compose run postgis psql
FLYWAY ?= docker-compose run flyway


# end export
# please keep that, it helps with autogenerating env wrappers

help: ## Ask for help!
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

all: install
	@echo prereqs that are newer than install: $?

install: ## Runs npm and bower install
	npm install
	bower install
	# hack to build purs deps only
	pulp build --src-path dapp/contracts

	cd graphql-server && npm install

############
# postgres
############
migrate: ## Run the flyway migration suite to setup postgis
	PGDATABASE=$(PG_BASE_DATABASE) $(PSQL) -h postgis -p 5432 -U $(PGUSER) -tc "SELECT 1 FROM pg_database WHERE datname = '$(PGDATABASE)'" | grep -q 1 || \
	PGDATABASE=$(PG_BASE_DATABASE) $(PSQL) -h postgis -p 5432 -U $(PGUSER) -c "CREATE DATABASE $(PGDATABASE);"
	# the -jarDirs is temporary fix for https://github.com/NixOS/nixpkgs/issues/59687
	$(FLYWAY) -user=$(PGUSER) -password=$(PGPASSWORD) -url=jdbc:postgresql://postgis:5432/$(PGDATABASE) -locations=filesystem:/flyway/sql/migrations -baselineOnMigrate=true migrate

show-migrations: ## Describe the migrations in the current database
	$(FLYWAY) -user=$(PGUSER) -password=$(PGPASSWORD) -url=jdbc:postgresql://postgis:5432/$(PGDATABASE) -locations=filesystem:/flyway/sql/migrations -baselineOnMigrate=true info

####################
# DAPP       #
####################

compile-contracts: ## Compile all contracts in dapp and write purescript ffi modules
	chanterelle compile && chanterelle codegen

build-dapp: ## Build the deploy script
	pulp build --src-path dapp/src

deploy-contracts: build-dapp ## Deploy contracts in dapp project
	chanterelle deploy ./output/Deploy.Main/index.js

test-dapp: ## Run the dapp unit test suite
	pulp test --src-path dapp/src --test-path dapp/test -m Test.Unit.Main -I frontend

test-e2e: ## Run the dapp unit test suite
	pulp test --src-path dapp/src --test-path dapp/test -m Test.E2E.Main -I frontend

clean-dapp: ## Clean up DApp related build artifacts
	rm -f build/**/*.json
	rm -rf output/Contracts.**
	rm -rf dapp/src/Contracts

####################
# Haskell      #
####################

haskell-deps: ## install supporting haskell-deps
	stack install hlint stylish-haskell

hlint: ## Run hlint on all haskell projects
	stack exec hlint -- -h .hlint.yaml server common indexer

stylish: ## Run stylish-haskell over all haskell projects
	find ./server ./common ./indexer -name "*.hs" | xargs stack exec stylish-haskell -- -c ./.stylish_haskell.yaml -i

####################
# SERVER       #
####################

build-server: ## build server and install binaries
	stack install server

run-server: build-server ## run the server
	stack exec -- server-exe

####################
# Indexer      #
####################

build-indexer: ## build indexer and install binaries
	stack install indexer

run-indexer: build-indexer ## run the indexer
	stack exec -- indexer-exe


####################
# frontend      #
####################

frontend-start: ## Starts webserver with livereload, (you might want to build all purescript sources first). Note that you should also be running PureScript IDE server which compiles PureScript files on change, If it's not the case for you run `make build-purs-watch` too.
	webpack-dev-server --port 3333 --hot --host 0.0.0.0

frontend-start-https: ## Same as `frontend-start`, but running with `https`
	webpack-dev-server --port 3333 --hot --host 0.0.0.0 --https

frontend-build: ## Builds css html and js assets.
	webpack

build-purs-strict: ## Build whole purescript src and test file in strict mode
	pulp build --jobs 8 --src-path frontend/src -I dapp/src -- --strict

build-purs: ## Build whole purescript src and test file
	pulp build --jobs 8 --src-path frontend/src -I dapp/src

purs-repl: ## Run a pulp repl against the purescript sources
	pulp repl --src-path frontend/src -I dapp/src

build-purs-watch: ## same as `make build-purs` but watches for changes for re-building
	pulp -w build --jobs 8 --src-path frontend/src -I dapp/src

build-purs-editor: ## Same as `make build-purs` but with json output, it's used in `purescript.buildCommand` of `.vscode/settings.json`, this could potentially be useful for Atom users too.
	pulp build --jobs 8 --src-path frontend/src -I dapp/src -- --json-errors

faucet-locally: ## Faucet some ETH locally
	pulp run --jobs 8 --src-path dapp/scripts -I dapp/src:frontend/src -m Faucet

signal-locally: ## Create signal locally
	pulp run --jobs 8 --src-path dapp/scripts -I dapp/src:frontend/src -m Signal
####################
# GraphQL Server #
####################

run-gql-server: ## run the graphql server
	cd graphql-server && npm run start

run-gql-dev: ## run the graphql server in dev mode
	cd graphql-server && npm run dev
