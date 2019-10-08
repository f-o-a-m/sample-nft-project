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
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (class DecodeJson, class EncodeJson, JCursor(..), Json, cursorGet, decodeJson, downField, encodeJson, stringify, (.:), (:=), (~>))
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
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
  encodeJson (GraphQLBody {query, variables}) = "query" := query ~> "variables" := variables


newtype Nodes a = Nodes { nodes :: Array a }
derive instance genericNodes :: Generic (Nodes a)  _
instance eqNodes :: (Eq a) => Eq (Nodes a) where eq = genericEq
instance decodeJsonNodes :: (DecodeJson a) => DecodeJson (Nodes a) where
  decodeJson = decodeJson >=> \obj -> Nodes <$> sequenceRecord { nodes: obj .: "nodes" }


mkGraphQLQuery :: GraphQLBody
               -> Aff Json
mkGraphQLQuery gbod = do
  let
    headers = AJ.defaultRequest.headers <> (maybe [] (\key -> [RequestHeader "Authorization" ("Bearer" <> key)]) graphQLApiKey)
    request = AJ.defaultRequest { url = graphQLApiUrl, responseFormat = ResponseFormat.json, content = Just (Json $ encodeJson gbod)}
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
foreign import graphQLApiKey :: Maybe String

newtype SignalOwnerStats = SignalOwnerStats
    { ethAddress            :: Address
    , numberOwned           :: BigNumber
    , numberOfPurchases     :: BigNumber
    , averagePurchasePrice  :: BigNumber
    , totalPurchases        :: BigNumber
    , numberOfSales         :: BigNumber
    , averageSalePrice      :: BigNumber
    , totalSales            :: BigNumber
    }
derive instance genericSignalOwnerStats :: Generic SignalOwnerStats _
instance eqSignalOwnerStats :: Eq SignalOwnerStats where eq = genericEq
instance showSignalOwnerStats :: Show SignalOwnerStats where show = genericShow
instance decodeJsonSignalOwnerStats :: DecodeJson SignalOwnerStats where
  decodeJson = decodeJson >=> \obj -> SignalOwnerStats <$> sequenceRecord
    { ethAddress: obj .: "ethAddress"
    , numberOwned: obj .: "numberOwned"
    , numberOfPurchases: obj .: "numberOfPurchases"
    , averagePurchasePrice: obj .: "averagePurchasePrice"
    , totalPurchases: obj .: "totalPurchases"
    , numberOfSales: obj .: "numberOfSales"
    , averageSalePrice: obj .: "averageSalePrice"
    , totalSales: obj .: "totalSales"
    }

type BlockRange =
  { startBlock :: Int
  , endBlock   :: Int
  }

getSignalOwnerStats :: BlockRange -> Cursor -> Aff (Collection SignalOwnerStats)
getSignalOwnerStats br c = do
  res <- mkGraphQLQuery $ soldStatsQuery br c
  let
    field = "signalOwnerStats"
    failMissingField = throwError $ error $ "missing " <> field <> " from " <> stringify res
    failInvalidJson = throwError $ error $ "Invalid JSON structure: " <> stringify res
  case cursorGet (downField field JCursorTop) res of
    Nothing -> failMissingField
    Just jsonStats -> do
      Nodes {nodes: stats} <- either (const failInvalidJson) pure (decodeJson jsonStats)
      pure {items: stats, next: Just {limit: c.limit, offset:c.offset + c.limit}}

soldStatsQuery :: BlockRange -> Cursor -> GraphQLBody
soldStatsQuery {startBlock, endBlock} {limit, offset} = GraphQLBody { query, variables }
 where
  variables = "startBlock" := startBlock
           ~> "endBlock"   := endBlock
           ~> "limit"      := limit
           ~> "offset"     := offset
  query =
    """
      query SignalOwnerStats(
        $limit: Int!
        $offset: Int!
        $startBlock: BigFloat
        $endBlock: BigFloat
      ) {
        signalOwnerStats(
          first: $limit
          offset: $offset
          startBlock: $startBlock
          endBlock: $endBlock
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


