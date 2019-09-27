{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module SignalMarket.Common.Models.SignalTokenTokensStaked where

import qualified Data.Aeson                     as A
import           Data.Profunctor.Product.TH     (makeAdaptorAndInstance)
import           GHC.Generics                   (Generic)
import qualified Katip                          as K
import           Opaleye                        (Field, SqlNumeric, SqlText,
                                                 Table, table, tableField)
import           SignalMarket.Common.Aeson      (defaultAesonOptions)
import           SignalMarket.Common.EventTypes (EthAddress, EventID, TokenID,
                                                 Value)

-- SignalToken
-- TokensStaked :: {_staker :: Address, _tokenId :: UIntN S256, _stakeAmount :: UIntN S256, _timestamp :: UIntN S256 }

data TokensStaked' staker tokenID stakeAmount timestamp eventID = TokensStaked
  { staker      :: staker
  , tokenID     :: tokenID
  , stakeAmount :: stakeAmount
  , timestamp   :: timestamp
  , eventID     :: eventID
  } deriving Generic

$(makeAdaptorAndInstance "pTokensStaked" ''TokensStaked')

type TokensStakedPG = TokensStaked' (Field SqlText) (Field SqlNumeric) (Field SqlNumeric) (Field SqlNumeric) (Field SqlText)
type TokensStaked = TokensStaked' EthAddress TokenID Value Value EventID

tokensStakedTable :: Table TokensStakedPG TokensStakedPG
tokensStakedTable = table "signal_token_tokens_staked"
                       (pTokensStaked TokensStaked { staker = tableField "staker"
                                                   , tokenID = tableField "token_id"
                                                   , stakeAmount = tableField "stake_amount"
                                                   , timestamp = tableField "timestamp"
                                                   , eventID = tableField "event_id"
                                                   }
                       )

instance A.ToJSON TokensStaked where
  toJSON = A.genericToJSON (defaultAesonOptions "")

instance A.FromJSON TokensStaked where
  parseJSON = A.genericParseJSON (defaultAesonOptions "")

instance K.ToObject TokensStaked

instance K.LogItem TokensStaked where
  payloadKeys _ _ = K.AllKeys
