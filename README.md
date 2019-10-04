# sample-nft-project
[![Build Status](https://travis-ci.com/f-o-a-m/sample-nft-project.svg?token=qCHTaxpXTk53j8Cxixyq&branch=master)](https://travis-ci.com/f-o-a-m/sample-nft-project)

A sample NFT marketplace for buying and selling FOAM Signals.

## Install Requirements
Requires `npm` and `stack`. This project uses [`chanterelle`](https://github.com/f-o-a-m/chanterelle) for deploying contracts and generating Purescript FFI bindings.

You'll also need `libpq` bindings and a postgres client. On Ubuntu run `apt-get install -y libpq-dev postgres-client`

## Makefile
There is a `Makefile` with commands for building different stages of the project and setting default environment variables. You can do

```bash
make help
```

to see a help menu.


### How to get some ETH locally?

- To get some ETH and FOAM add your MetaMask address to `FAUCET_ADDRESS` and run
```bash
FAUCET_ADDRESS={YOUR_METAMASK_ACCOUNT} make faucet-locally
```
- To get more FOAM, use `FAUCET_VALUE` to add any value you want, e.g.
```bash
FAUCET_ADDRESS={YOUR_METAMASK_ACCOUNT} FAUCET_VALUE=224455 make faucet-locally
```
