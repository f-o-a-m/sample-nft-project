module App.GraphQLApi
  ( getSignalOwnerStats
  ) where

import Prelude

import Affjax (printResponseFormatError)
import Affjax as AJ
import Affjax.RequestBody (RequestBody(..))
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import App.Data.Collections (Cursor, Collection)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (class DecodeJson, class EncodeJson, JCursor(..), Json, cursorGet, decodeJson, downField, encodeJson, stringify, (:=), (~>))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (Minutes(..), fromDuration)
import Effect.Aff (Aff)
import Effect.Exception (error)
import Foreign.Generic (genericDecodeJSON)
import Network.Ethereum.Core.BigNumber (BigNumber)
import Network.Ethereum.Core.Signatures (Address)

type URLPath = String

newtype GraphQLBody = GraphQLBody
      { query :: String
      , variables :: Json
      }
derive instance genericGraphQLBody :: Generic GraphQLBody  _
instance eqGraphQLBody :: Eq GraphQLBody where eq = genericEq
instance showGraphQLBody :: Show GraphQLBody where show = genericShow
instance encodeJsonGraphQLBody :: EncodeJson GraphQLBody where
  encodeJson (GraphQLBody {query, variables}) = "query" := query ~> "variables" := variables


newtype Nodes = Nodes { nodes :: Array a }
derive instance genericNodes :: (Generic a) => Generic (Nodes a)  _
instance eqNodes :: (Generic a) => Eq (Nodes a) where eq = genericEq
instance showNodes :: Show (Nodes a) where show = genericShow
instance decodeJsonNodes :: (DecodeJson a) => DecodeJson (Nodes a) where
  decodeJson = genericDecodeJSON


mkGraphQLQuery :: forall a .
                  DecodeJson a
               => GraphQLBody a
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
      let eData = maybe (Left "Missing data field from") Right (cursorGet (downField "data" JCursorTop) resp.body)
          eErrors = maybe (Right unit) (\errs -> Left stringify errs) (cursorGet (downField "errors" JCursorTop) resp.body)
      in case {eData, eErrors} of
          {eData: Left err} -> throwAPIError resp "No data or errors field found on response body." $ printResponseFormatError err
          {eErrors: Left err} -> throwAPIError resp "No data or errors field found on response body." $ printResponseFormatError err
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
instance eqSignalOwnerStats :: Eq SignalOwnerStats where eq = genericEq
instance showSignalOwnerStats :: Show SignalOwnerStats where show = genericShow
instance decodeJsonSignalOwnerStats :: DecodeJson SignalOwnerStats where
  decodeJson  = genericDecodeJSON

getSignalOwnerStats :: BlockRange -> Cursor -> Aff (Collection SignalOwnerStats)
getSignalOwnerStats br c = do
  let
    field = "signalOwnerStats"
    failure = throwError $ error $ "missing " <> field <> " from " <> show res
  res <- mkGraphQLQuery $ soldStatsQuery br c
  Node {nodes: stats} <- maybe failure $ pure $ (decodeJson =<< cursorGet (downField field JCursorTop) res)
  pure stats


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


