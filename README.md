# sample-nft-project
[![Build Status](https://travis-ci.com/f-o-a-m/sample-nft-project.svg?token=qCHTaxpXTk53j8Cxixyq&branch=master)](https://travis-ci.com/f-o-a-m/sample-nft-project)

A sample NFT marketplace for buying and selling FOAM Signals.

## Install Requirements
Requires `npm` and `stack`. This project uses [`chanterelle`](https://github.com/f-o-a-m/chanterelle) for deploying contracts and generating Purescript FFI bindings.

You'll also need `libpq` bindings. On Ubuntu run `apt-get install -y libpq-dev`

## Makefile
There is a `Makefile` with commands for building different stages of the project and setting default environment variables. You can do

```bash
make help
```

to see a help menu.

## Deploying

### 1. Install dependencies
`> make install`

### 2. Compile smart contracts and generate bindings
`> make compile-contracts`

### 3. Start services
`> docker-compose up -d`

### 4. Deploy contracts to ethereum
`> make deploy-contracts`

### 5. Build the backend
`> make build-server build-indexer`

### 6. Build the frontend
`> make build-purs build-dapp frontend-build`

### 7. Start the app
In three separate terminals, run
- `> make run-indexer`
- `> make run-server`
- `> make frontend-start`

## Detailed documentation

More detailed documentation are available for the following components:
- [server](https://github.com/f-o-a-m/sample-nft-project/blob/master/server/README.md)
- [indexer](https://github.com/f-o-a-m/sample-nft-project/blob/master/indexer/README.md)


