module App.Error where

import Prelude

import Network.Ethereum.Web3 (CallError(..), Web3Error(..))
import Network.Ethereum.Web3.Types (RpcError(..))

printWeb3Error :: Web3Error -> String
printWeb3Error = case _ of
  Rpc (RpcError { code, message }) -> "Got RpcError with code: `" <> show code <> "` and message: `" <> show message <> "`"
  RemoteError err -> "Got RemoteError with message: " <> show err
  ParserError err -> "Got ParserError with message: " <> show err
  NullError -> "Got NullError"

printCallError :: CallError -> String
printCallError (NullStorageError {signature, _data})= "Got CallError with signature: `" <> show signature <> "` and _data: `" <> show _data <> "`"
