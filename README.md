# sample-nft-project
[![Build Status](https://travis-ci.com/f-o-a-m/sample-nft-project.svg?token=qCHTaxpXTk53j8Cxixyq&branch=master)](https://travis-ci.com/f-o-a-m/sample-nft-project)

A sample NFT marketplace for buying and selling FOAM Signals.

## Install
Requires `npm` and `bower`. This project also requires [`chanterelle`](https://github.com/f-o-a-m/chanterelle) for deploying contracts and generating Purescript FFI bindings. 

Install dependencies via `npm` and `bower`.

```
npm install && bower install
```

Compile solidity contracts via `chanterelle`, then build the project.

```
npm run compile-contracts && npm run build
```

## Testing 

To run tests, run a docker image for [`Cliquebait`](https://github.com/f-o-a-m/cliquebait).

```
docker run -d -p 8545:8545 foamspace/cliquebait:latest
```

Then run the tests.

```
npm run test
```

## Deploy

`chanterelle deploy ./output/Deploy.Main/index.js`
