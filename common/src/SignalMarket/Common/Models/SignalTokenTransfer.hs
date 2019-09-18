{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module SignalMarket.Common.Models.SignalTokenTransfer where

import qualified Data.Aeson                      as A
import           Data.Int                        (Int64)
import           Data.Profunctor.Product.Default
import           Data.Profunctor.Product.TH      (makeAdaptorAndInstance)
import           Database.PostgreSQL.Simple      (Connection)
import           GHC.Generics                    (Generic)
import qualified Katip                           as K
import           Opaleye                         (Field, SqlNumeric, SqlText,
                                                  Table, ToFields, table,
                                                  tableField)
import           SignalMarket.Common.Aeson       (defaultAesonOptions)
import           SignalMarket.Common.EventTypes  (EthAddress, EventID, TokenID)

-- SignalToken
-- Transfer :: {_from :: Address,_to :: Address,_tokenId :: (UIntN (D2 :& D5 :& DOne D6))}

data Transfer' to from tokenID eventID = Transfer
  { to      :: to
  , from    :: from
  , tokenID :: tokenID
  , eventID :: eventID
  } deriving Generic

$(makeAdaptorAndInstance "pTransfer" ''Transfer')

type TransferPG = Transfer' (Field SqlText) (Field SqlText) (Field SqlNumeric) (Field SqlText)
type Transfer = Transfer' EthAddress EthAddress TokenID EventID

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
