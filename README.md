# sample-nft-project
[![Build Status](https://travis-ci.com/f-o-a-m/sample-nft-project.svg?token=qCHTaxpXTk53j8Cxixyq&branch=master)](https://travis-ci.com/f-o-a-m/sample-nft-project)

A sample NFT marketplace for buying and selling FOAM Signals.

## Install Requirements
Requires `npm`, `node` and [`stack`](http://haskellstack.org). This project uses [`chanterelle`](https://github.com/f-o-a-m/chanterelle) for deploying contracts and generating Purescript FFI bindings.

You'll also need `libpq` bindings. On Ubuntu run

```sh
apt-get install -y libpq-dev
```

## Makefile
There is a `Makefile` with commands for building different stages of the project and setting default environment variables. You can do

```sh
make help
```

to see a help menu.

## Deploying

### 1. Install dependencies

```sh
make install
```

### 2. Compile smart contracts and generate bindings

```sh
make compile-contracts
```

### 3. Start services and run migrations
Note that both table creation and migration runs from within docker, to avoid having to install `psql` and `flyway` on the host machine.

```sh
docker-compose up -d
make migrate
```

### 4. Deploy contracts to ethereum

```sh
make deploy-contracts
```

### 5. Build the backend

```sh
make build-server build-indexer
```

### 6. Build the frontend

```sh
make build-purs build-dapp frontend-build
```

### 7. Start the app
In three separate terminals, run

```sh
make run-indexer
```

```sh
make run-server
```

```sh
make frontend-start
```

## Detailed documentation

More detailed documentation are available for the following components:
- [server](https://github.com/f-o-a-m/sample-nft-project/blob/master/server/README.md)
- [indexer](https://github.com/f-o-a-m/sample-nft-project/blob/master/indexer/README.md)

## Testing

The application comes with both a unit and an end-to-end test suite.

### Unit tests
To run the unit-tests, make sure that the docker services are running:

```sh
docker-compose up -d
```

Then run:

```sh
make test-dapp
```

This will create an _ephemeral_ deployment of the smart contracts and run tests against them.

### End-to-end tests
The end-to-end tests demonstrate the full lifecycle of the different components of the stack. To run them, you should have a deployment already by following steps 1-7 above (you can skip `make frontend-start`). Once the contracts are deployed and the indexer and the server are running, run:

```sh
make test-e2e
```

The tests are written in PureScript and will set up listeners from Ethereum, the server as websockets (via the indexer) and will trigger contract interactions and test the pipeline by anticipating the correct response from the services. It can also compare the data from the services with that emitted as events from Ethereum itself.


## Other

### How to get some ETH locally?

- To get some ETH and FOAM add your MetaMask address to `FAUCET_ADDRESS` and run
```bash
FAUCET_ADDRESS={YOUR_METAMASK_ACCOUNT} make faucet-locally
```
- To get more FOAM, use `FAUCET_VALUE` to add any value you want, e.g.
```bash
FAUCET_ADDRESS={YOUR_METAMASK_ACCOUNT} FAUCET_VALUE=224455 make faucet-locally
```
