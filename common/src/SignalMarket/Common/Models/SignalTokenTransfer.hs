{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module SignalMarket.Common.Models.SignalTokenTransfer where

import           Data.Swagger                   (SwaggerType (..),
                                                 ToParamSchema (..),
                                                 ToSchema (..),
                                                 defaultSchemaOptions,
                                                 genericDeclareNamedSchema)

import qualified Data.Aeson                     as A
import           Data.Profunctor.Product.TH     (makeAdaptorAndInstance)
import           GHC.Generics                   (Generic)
import qualified Katip                          as K
import           Opaleye                        (Field, SqlNumeric, SqlText,
                                                 Table, table, tableField)
import           SignalMarket.Common.Aeson      (defaultAesonOptions)
import           SignalMarket.Common.EventTypes (EthAddress, EventID, TokenID)

-- | Represents the transfer of a signal token from one owner to the next.
data Transfer' to from tokenID eventID = Transfer
  { to      :: to
  , from    :: from
  , tokenID :: tokenID
  , eventID :: eventID
  } deriving Generic

$(makeAdaptorAndInstance "pTransfer" ''Transfer')

type TransferPG = Transfer' (Field SqlText) (Field SqlText) (Field SqlNumeric) (Field SqlText)
type Transfer = Transfer' EthAddress EthAddress TokenID EventID

instance ToSchema Transfer where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy


transferTable :: Table TransferPG TransferPG
transferTable = table "signal_token_transfer"
                       (pTransfer Transfer { to = tableField "to"
                                           , from  = tableField "from"
                                           , tokenID = tableField "token_id"
                                           , eventID = tableField "event_id"
                                           }
                       )

instance A.ToJSON Transfer where
  toJSON = A.genericToJSON (defaultAesonOptions "")

instance A.FromJSON Transfer where
  parseJSON = A.genericParseJSON (defaultAesonOptions "")

instance K.ToObject Transfer

instance K.LogItem Transfer where
  payloadKeys _ _ = K.AllKeys
