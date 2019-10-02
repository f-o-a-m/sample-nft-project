{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module SignalMarket.Common.Models.SignalTokenTokensUnstaked where

import qualified Data.Aeson                     as A
import           Data.Profunctor.Product.TH     (makeAdaptorAndInstance)
import           GHC.Generics                   (Generic)
import qualified Katip                          as K
import           Opaleye                        (Field, SqlNumeric, SqlText,
                                                 Table, table, tableField)
import           SignalMarket.Common.Aeson      (defaultAesonOptions)
import           SignalMarket.Common.EventTypes (EthAddress, EventID, TokenID,
                                                 Value)

-- | Represents a signal token that has been removed and it's stake returned to the owner.
data TokensUnstaked' staker tokenID stakeAmount timestamp eventID = TokensUnstaked
  { staker      :: staker
  , tokenID     :: tokenID
  , stakeAmount :: stakeAmount
  , timestamp   :: timestamp
  , eventID     :: eventID
  } deriving Generic

$(makeAdaptorAndInstance "pTokensUnstaked" ''TokensUnstaked')

type TokensUnstakedPG = TokensUnstaked' (Field SqlText) (Field SqlNumeric) (Field SqlNumeric) (Field SqlNumeric) (Field SqlText)
type TokensUnstaked = TokensUnstaked' EthAddress TokenID Value Value EventID

tokensUnstakedTable :: Table TokensUnstakedPG TokensUnstakedPG
tokensUnstakedTable = table "signal_token_tokens_unstaked"
                       (pTokensUnstaked TokensUnstaked { staker = tableField "staker"
                                                   , tokenID = tableField "token_id"
                                                   , stakeAmount = tableField "stake_amount"
                                                   , timestamp = tableField "timestamp"
                                                   , eventID = tableField "event_id"
                                                   }
                       )

instance A.ToJSON TokensUnstaked where
  toJSON = A.genericToJSON (defaultAesonOptions "")

instance A.FromJSON TokensUnstaked where
  parseJSON = A.genericParseJSON (defaultAesonOptions "")

instance K.ToObject TokensUnstaked

instance K.LogItem TokensUnstaked where
  payloadKeys _ _ = K.AllKeys
