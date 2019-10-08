module App.GraphQLApi
  ( getSignalOwnerStats
  , SignalOwnerStats(..)
  ) where

import Prelude

import Affjax (printResponseFormatError)
import Affjax as AJ
import Affjax.RequestBody (RequestBody(..))
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import App.Data.Collections (Cursor, Collection)
import App.Data.SignalOwnerStats (SignalOwnerStats(..))
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (class DecodeJson, class EncodeJson, JCursor(..), Json, cursorGet, decodeJson, downField, encodeJson, stringify, (.:), (:=), (~>))
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (Minutes(..), fromDuration)
import Effect.Aff (Aff)
import Effect.Exception (error)
import Network.Ethereum.Core.BigNumber (BigNumber)
import Network.Ethereum.Core.Signatures (Address)
import Record.Extra (sequenceRecord)


newtype GraphQLBody = GraphQLBody
      { query :: String
      , variables :: Json
      }
derive instance genericGraphQLBody :: Generic GraphQLBody  _
instance eqGraphQLBody :: Eq GraphQLBody where eq = genericEq
instance encodeJsonGraphQLBody :: EncodeJson GraphQLBody where
  encodeJson (GraphQLBody {query, variables}) = encodeJson {query, variables}


newtype Nodes a = Nodes { nodes :: Array a }
derive instance genericNodes :: Generic (Nodes a)  _
instance eqNodes :: (Eq a) => Eq (Nodes a) where eq = genericEq
instance decodeJsonNodes :: (DecodeJson a) => DecodeJson (Nodes a) where
  decodeJson = decodeJson >=> \obj -> Nodes <$> sequenceRecord { nodes: obj .: "nodes" }


mkGraphQLQuery :: GraphQLBody
               -> Aff Json
mkGraphQLQuery gbod = do
  let
    headers = AJ.defaultRequest.headers <> ([RequestHeader "Authorization" ("Bearer " <> graphQLApiKey)])
    request = AJ.defaultRequest { url = graphQLApiUrl, responseFormat = ResponseFormat.json, content = Just (Json $ encodeJson gbod), method = Left POST, headers = headers}
    retryPolicy = AJ.defaultRetryPolicy {timeout = Just $ fromDuration $ Minutes 0.4}
  resp <- AJ.retry retryPolicy AJ.request request
  case resp.body of
    Left err -> throwAPIError resp "AffJax error" $ printResponseFormatError err
    Right json ->
      let eData = maybe (Left "Missing data field from") Right (cursorGet (downField "data" JCursorTop) json)
          eErrors = maybe (Right unit) (\errs -> Left (stringify errs)) (cursorGet (downField "errors" JCursorTop) json)
      in case {eData, eErrors} of
          {eData: Left err} -> throwAPIError resp "No data or errors field found on response body." err
          {eErrors: Left err} -> throwAPIError resp "No data or errors field found on response body." err
          {eData: Right jsonBody} -> pure jsonBody
 where
  throwAPIError resp msg err = throwError $ error $ msg <> ": " <> show
    { error: err
    , status: resp.status
    , statusText: resp.statusText
    , headers: resp.headers
    }

foreign import graphQLApiUrl :: String
foreign import graphQLApiKey :: String

getSignalOwnerStats :: Cursor -> Aff (Collection SignalOwnerStats)
getSignalOwnerStats c = do
  res <- mkGraphQLQuery $ soldStatsQuery c
  let failInvalidJson err = throwError $ error $ err <> " "  <> stringify res
  { signalOwnerStats: Nodes { nodes: stats  } } :: {signalOwnerStats :: Nodes SignalOwnerStats} <- either failInvalidJson pure (decodeJson res)
  pure {items: stats, next: Just {limit: c.limit, offset:c.offset + c.limit}}

soldStatsQuery :: Cursor -> GraphQLBody
soldStatsQuery cursor = GraphQLBody { query, variables }
 where
  variables = encodeJson cursor
  query =
    """
      query SignalOwnerStats(
        $limit: Int!
        $offset: Int!
      ) {
        signalOwnerStats(
          first: $limit
          offset: $offset
        ) {
          nodes {
            ethAddress
            numberOwned
            numberOfPurchases
            averagePurchasePrice
            totalPurchases
            numberOfSales
            averageSalePrice
            totalSales
          }
        }
      }
    """


