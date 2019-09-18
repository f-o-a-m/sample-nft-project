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

PG_BASE_DATABASE ?= postgres 
PGDATABASE ?= signal_market
PGHOST ?= localhost
PGPORT ?= 5432
PGPASSWORD ?= password
PGUSER ?= postgres

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

############
# postgres
############
migrate: ## Run the flyway migration suite to setup postgis
	PGDATABASE=$(PG_BASE_DATABASE) psql -tc "SELECT 1 FROM pg_database WHERE datname = '$(PGDATABASE)'" | grep -q 1 || \
	PGDATABASE=$(PG_BASE_DATABASE) psql -c "CREATE DATABASE $(PGDATABASE);"
	# the -jarDirs is temporary fix for https://github.com/NixOS/nixpkgs/issues/59687
	flyway -user=$(PGUSER) -password=$(PGPASSWORD) -url=jdbc:postgresql://$(PGHOST):$(PGPORT)/$(PGDATABASE) -locations=filesystem:migrations -baselineOnMigrate=true migrate

####################
# DAPP       #
####################

compile-contracts: ## Compile all contracts in dapp and write purescript ffi modules
	chanterelle compile && chanterelle codegen

build-dapp: ## Build the deploy script
	pulp  build --src-path dapp/src

deploy-contracts: ## Deploy contracts in dapp project
	chanterelle deploy ./output/Deploy.Main/index.js

test-dapp: ## Run the dapp test suite
	pulp test --src-path dapp/src --test-path dapp/test -m Test.Main

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
