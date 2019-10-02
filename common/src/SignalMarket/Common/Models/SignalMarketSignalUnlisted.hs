{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module SignalMarket.Common.Models.SignalMarketSignalUnlisted where

import           Data.Profunctor.Product.TH     (makeAdaptorAndInstance)
import           Opaleye                        (Field, SqlNumeric, SqlText, Table,
                                                 table, tableField)
import           SignalMarket.Common.EventTypes (SaleID, TokenID, EventID)
import           SignalMarket.Common.Aeson
import qualified Katip as K
import qualified Data.Aeson as A
import GHC.Generics (Generic)

-- | Represents a sale that was unlisted by the owner.
data SignalUnlisted' saleID tokenID eventID = SignalUnlisted
  { saleID :: saleID
  , tokenID :: tokenID
  , eventID :: eventID
  } deriving Generic

$(makeAdaptorAndInstance "pSignalUnlisted" ''SignalUnlisted')

type SignalUnlistedPG = SignalUnlisted' (Field SqlNumeric) (Field SqlNumeric) (Field SqlText)
type SignalUnlisted = SignalUnlisted' SaleID TokenID EventID

signalUnlistedTable :: Table SignalUnlistedPG SignalUnlistedPG
signalUnlistedTable = table "signal_unlisted"
                            (pSignalUnlisted SignalUnlisted { saleID = tableField "sale_id"
                                                            , tokenID = tableField "token_id"
                                                            , eventID = tableField "event_id"
                                                            }
                            )

instance A.ToJSON SignalUnlisted where
  toJSON = A.genericToJSON (defaultAesonOptions "")

instance A.FromJSON SignalUnlisted where
  parseJSON = A.genericParseJSON (defaultAesonOptions "")

instance K.ToObject SignalUnlisted

instance K.LogItem SignalUnlisted where
  payloadKeys _ _ = K.AllKeys
