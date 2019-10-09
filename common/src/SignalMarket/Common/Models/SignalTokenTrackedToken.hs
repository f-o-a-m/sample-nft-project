{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module SignalMarket.Common.Models.SignalTokenTrackedToken where

import           Data.Swagger                   (SwaggerType (..),
                                                 ToParamSchema (..),
                                                 ToSchema (..),
                                                 defaultSchemaOptions,
                                                 genericDeclareNamedSchema)

import qualified Data.Aeson                     as A
import           Data.Profunctor.Product.TH     (makeAdaptorAndInstance)
import           GHC.Generics                   (Generic)
import qualified Katip                          as K
import           Opaleye                        (Field, SqlBool, SqlNumeric,
                                                 SqlText, Table, table,
                                                 tableField)
import           SignalMarket.Common.Aeson      (defaultAesonOptions)
import           SignalMarket.Common.EventTypes (ByteNValue, EthAddress,
                                                 EventID, HexInteger, TokenID,
                                                 Value)

-- represents a signal token with the data about the signal
data TrackedToken' nftAddress cst geohash radius tokenID owner staked burned eventID = TrackedToken
  { nftAddress :: nftAddress
  , cst        :: cst
  , geohash    :: geohash
  , radius     :: radius
  , tokenID    :: tokenID
  , owner      :: owner
  , staked     :: staked
  , isBurned   :: burned
  , eventID    :: eventID
  } deriving Generic

$(makeAdaptorAndInstance "pTrackedToken" ''TrackedToken')

type TrackedTokenPG =
  TrackedToken' (Field SqlText) (Field SqlText) (Field SqlText) (Field SqlNumeric) (Field SqlNumeric) (Field SqlText) (Field SqlNumeric) (Field SqlBool) (Field SqlText)
type TrackedToken = TrackedToken' EthAddress ByteNValue ByteNValue HexInteger TokenID EthAddress Value Bool EventID

instance ToSchema TrackedToken where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy

trackedTokenTable :: Table TrackedTokenPG TrackedTokenPG
trackedTokenTable = table "signal_token_tracked_token"
                           (pTrackedToken TrackedToken { nftAddress = tableField "nft_address"
                                                       , cst = tableField "cst"
                                                       , geohash = tableField "geohash"
                                                       , radius = tableField "radius"
                                                       , tokenID = tableField "token_id"
                                                       , owner = tableField "owner"
                                                       , staked = tableField "staked"
                                                       , isBurned = tableField "is_burned"
                                                       , eventID = tableField "event_id"
                                                       }
                           )

instance A.ToJSON TrackedToken where
  toJSON = A.genericToJSON (defaultAesonOptions "")

instance A.FromJSON TrackedToken where
  parseJSON = A.genericParseJSON (defaultAesonOptions "")

instance K.ToObject TrackedToken

instance K.LogItem TrackedToken where
  payloadKeys _ _ = K.AllKeys
