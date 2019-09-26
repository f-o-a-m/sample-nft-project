module App.API.Routes (getContracts) where

import Prelude

import Affjax (printResponseFormatError)
import Affjax as AJ
import Affjax.ResponseFormat as ResponseFormat
import App.Data.Contracts (Contracts)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (class DecodeJson, decodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Minutes(..), fromDuration)
import Effect.Aff (Aff)
import Effect.Exception (error)

getContracts :: Aff Contracts
getContracts = get "contracts"

type URLPath = String

get :: forall a. DecodeJson a => URLPath -> Aff a
get path = do
  let
    request = AJ.defaultRequest { url = apiBaseURL <> path, responseFormat = ResponseFormat.json }
    retryPolicy = AJ.defaultRetryPolicy {timeout = Just $ fromDuration $ Minutes 0.4}
  resp <- AJ.retry retryPolicy AJ.request request
  -- resp <- AJ.request request
  case resp.body of
    Left err -> throwAPIError resp "AffJax error" $ printResponseFormatError err
    Right json -> case decodeJson json of
      Left err -> throwAPIError resp "DecodeJson error" err
      Right res -> pure res
  where
    throwAPIError resp msg err = throwError $ error $ msg <> ": " <> show
      { error: err
      , status: resp.status
      , statusText: resp.statusText
      , headers: resp.headers
      }

foreign import apiBaseURL :: String
