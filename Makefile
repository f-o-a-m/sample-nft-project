.PHONY: help install watch compile-contracts build-dapp deploy-contracts test-dapp build-server
.DEFAULT_GOAL := help

export

# see https://stackoverflow.com/a/26936855/1798418
PATH  := node_modules/.bin:$(PATH)
SHELL := /bin/bash

NODE_URL ?= http://localhost:8545
ABIS_DIR ?= build

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
# SERVER       #
####################

build-server: ## build server and install binaries
	stack install server
