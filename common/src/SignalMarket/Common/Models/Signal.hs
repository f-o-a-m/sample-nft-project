{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
module SignalMarket.Common.Models.Signal where

import qualified Data.Aeson                     as A
import           Data.Profunctor.Product.TH     (makeAdaptorAndInstance)
import           GHC.Generics                   (Generic)
import qualified Katip                          as K
import           Opaleye                        (Field, FieldNullable,
                                                 SqlNumeric, SqlText, Table,
                                                 table, tableField)
import           SignalMarket.Common.Aeson      (defaultAesonOptions)
import           SignalMarket.Common.EventTypes (EthAddress, EventID, TokenID,
                                                 Value)

-- Signal cache table, for easily figuring out the owner and stake of a signal

data Signal' tokenID owner amountStaked tokensStakedEID tokensUnstakedEID trackedTokenEID lastTransferEID mintingTransferEID = Signal
  { tokenID            :: tokenID
  , owner              :: owner
  , amountStaked       :: amountStaked
  , tokensStakedEID    :: tokensStakedEID
  , tokensUnstakedEID  :: tokensUnstakedEID
  , trackedTokenEID    :: trackedTokenEID
  , lastTransferEID    :: lastTransferEID
  , mintingTransferEID :: mintingTransferEID
  } deriving Generic

$(makeAdaptorAndInstance "pSignal" ''Signal')

type SignalPG = Signal' (Field SqlNumeric) (Field SqlText) (Field SqlNumeric) (FieldNullable SqlText) (FieldNullable SqlText) (FieldNullable SqlText) (Field SqlText) (Field SqlText)
type Signal = Signal' TokenID EthAddress Value (Maybe EventID) (Maybe EventID) (Maybe EventID) EventID EventID

signalsTable :: Table SignalPG SignalPG
signalsTable = table "signals"
                       (pSignal Signal { tokenID            = tableField "token_id"
                                       , owner              = tableField "owner"
                                       , amountStaked       = tableField "amount_staked"
                                       , tokensStakedEID    = tableField "tokens_staked"
                                       , tokensUnstakedEID  = tableField "tokens_unstaked"
                                       , trackedTokenEID    = tableField "tracked_token"
                                       , lastTransferEID    = tableField "last_transfer"
                                       , mintingTransferEID = tableField "minting_transfer"
                                       }
                       )

instance A.ToJSON Signal where
  toJSON = A.genericToJSON (defaultAesonOptions "")

instance A.FromJSON Signal where
  parseJSON = A.genericParseJSON (defaultAesonOptions "")

instance K.ToObject Signal

instance K.LogItem Signal where
  payloadKeys _ _ = K.AllKeys
