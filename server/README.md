# server

## Running
The `Makefile` in the repository root contains the commands for building, running, and testing the server. By default the server runs on port `9000`, but can be overwritten by the `SERVER_PORT` environment variable.

The server assumes that you have run `make deploy-contracts` and that your chanterelle artifacts are sitting in the `ABI_DIRS` directory which defaults to `./build` relative to the root directory. For running locally, you don't need to worry about this.

You can test the server running

```bash
> make run-server
> curl localhost:9000/config/contracts
```

you should get a response like

```json
{
  "signalToken": {
    "blockHash": "0x8dc48017468354f4892a726510869da2daf767979684d04fdb81594fb1bb6741",
    "transactionHash": "0x753d7b1093238d80bcd101b73ea919d19bb18c9c94047653407e49eb30e6be70",
    "address": "0x49c015410bf2ccb1fb1217260685f61e924d3614",
    "blockNumber": "0x2ad1"
  },
  "foamToken": {
    "blockHash": "0xbbd89c472681fc840cda28dff0860e1a0e89a24b1b089841117e38eba719029c",
    "transactionHash": "0x0801220c614ecade49d7dad41cd8d0e5152e972041f270b88e27349f6cd1238f",
    "address": "0xa13d377548adf496a2123e6bd4599994c1ef6061",
    "blockNumber": "0x2acd"
  },
  "signalMarket": {
    "blockHash": "0x4492df26465e404bf59a042c1517bd03664fbdd92e809aeea4a8e1a05fecf97c",
    "transactionHash": "0xcd6cb378c28529ddd1a431395bf7f5bb641bc6fc71db09063a08b23252e1010c",
    "address": "0x285b7cba53104627096fa3efa3a6503de18498cf",
    "blockNumber": "0x2ad5"
  }
}

```
