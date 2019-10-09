module App.Data.Event where

import Prelude

import App.Data.Radius (Radius(..))
import App.Data.SaleId (SaleId(..))
import App.Data.Signal (Signal(..))
import App.Data.SignalActivity (SignalActivity(..))
import App.Data.SignalId (SignalId(..))
import App.Data.Token (Token(..))
import Contracts.SignalMarket as SignalMarket
import Contracts.SignalToken as SignalToken
import Control.MonadZero (guard)
import Data.Geohash (geohashFromBS)
import Data.Maybe (Maybe(..))
import Network.Ethereum.Web3 (unBytesN)


data Event
  = SignalForSale SignalMarket.SignalForSale
  | SignalUnlisted SignalMarket.SignalUnlisted
  | SignalSold SignalMarket.SignalSold
  | TrackedToken SignalToken.TrackedToken

eventToSignalId :: Event -> Maybe SignalId
eventToSignalId = case _ of
  TrackedToken (SignalToken.TrackedToken e) -> Nothing
  SignalForSale (SignalMarket.SignalForSale e) -> Just $ SignalId e.tokenId
  SignalUnlisted (SignalMarket.SignalUnlisted e) -> Just $ SignalId e.tokenId
  SignalSold (SignalMarket.SignalSold e) -> Just $ SignalId e.tokenId

eventToSignal :: Event -> Maybe Signal
eventToSignal = case _ of
  TrackedToken (SignalToken.TrackedToken e) -> Just $ Signal
    { id: SignalId e.tokenID
    , stake: Token e.stake
    , owner: e.owner
    , geohash: geohashFromBS $ unBytesN e.geohash
    , radius: Radius e.radius
    , sale: Nothing
    }
  SignalForSale (SignalMarket.SignalForSale e) -> Nothing
  SignalUnlisted (SignalMarket.SignalUnlisted e) -> Nothing
  SignalSold (SignalMarket.SignalSold e) -> Nothing


eventToSignalUpdate :: Event -> Signal -> Maybe Signal
eventToSignalUpdate = case _ of
  TrackedToken (SignalToken.TrackedToken e) -> \_ -> Nothing
  SignalForSale (SignalMarket.SignalForSale e) -> \(Signal s) -> do
    guard $ s.id == SignalId e.tokenId
    pure $ Signal s
      { sale = Just {price: Token e.price, id: SaleId e.saleId}
      }
  SignalUnlisted (SignalMarket.SignalUnlisted e) -> \(Signal s) -> do
    guard $ s.id == SignalId e.tokenId
    pure $ Signal s
      { sale = Nothing
      }
  SignalSold (SignalMarket.SignalSold e) -> \(Signal s) -> do
    guard $ s.id == SignalId e.tokenId
    pure $ Signal s
      { owner = e.newOwner, sale = Nothing
      }

eventToSignalActivity :: Event -> Maybe SignalActivity
eventToSignalActivity = case _ of
  TrackedToken (SignalToken.TrackedToken e) -> Nothing
  SignalForSale (SignalMarket.SignalForSale e) ->
    Just $ ListedForSale
      { owner: e.seller
      , price: Token e.price
      , saleId: SaleId e.saleId
      }
  SignalUnlisted (SignalMarket.SignalUnlisted e) ->
    Just $ UnlistedFromSale
      { owner: e.owner
      , saleId: SaleId e.saleId
      }
  SignalSold (SignalMarket.SignalSold e) ->
    Just $ Sold
      { owner: e.owner
      , buyer: e.newOwner
      , saleId: SaleId e.saleId
      , price: Token e.price
      }
