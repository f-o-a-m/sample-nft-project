module App.Data.Event where

import Prelude

import App.Data.Activity (Activity(..))
import App.Data.Radius (Radius(..))
import App.Data.SaleId (SaleId(..))
import App.Data.Signal (Signal(..))
import App.Data.SignalActivity (SignalActivity(..))
import App.Data.SignalId (SignalId(..))
import App.Data.Token (Token(..))
import Contracts.FoamToken as FoamToken
import Contracts.SignalMarket as SignalMarket
import Contracts.SignalToken as SignalToken
import Control.Monad.Reader (ReaderT, ask, lift)
import Control.MonadZero (guard)
import Data.Geohash (geohashFromBS)
import Data.Maybe (Maybe(..))
import Data.Variant as V
import Network.Ethereum.Web3 (unBytesN)

-- TODO we can flatten this
type Event =
  { raw :: EventRaw
  }

type EventRaw = V.Variant
  ( transfer :: FoamToken.Transfer
  , signalForSale :: SignalMarket.SignalForSale
  , signalUnlisted :: SignalMarket.SignalUnlisted
  , signalSold :: SignalMarket.SignalSold
  , trackedToken :: SignalToken.TrackedToken
  )

eventToSignalId :: Event -> Maybe SignalId
eventToSignalId = _.raw >>> V.match
  { transfer: \(FoamToken.Transfer e) -> Nothing
  , trackedToken: \(SignalToken.TrackedToken e) -> Nothing
  , signalForSale: \(SignalMarket.SignalForSale e) -> Just $ SignalId e.tokenId
  , signalUnlisted: \(SignalMarket.SignalUnlisted e) -> Just $ SignalId e.tokenId
  , signalSold: \(SignalMarket.SignalSold e) -> Just $ SignalId e.tokenId
  }

eventToSignal :: Event -> Maybe Signal
eventToSignal = _.raw >>> V.match
  { transfer: \(FoamToken.Transfer e) -> Nothing
  , trackedToken: \(SignalToken.TrackedToken e) -> Just $ Signal
      { id: SignalId e.tokenID
      , stake: Token e.stake
      , owner: e.owner
      , geohash: geohashFromBS $ unBytesN e.geohash
      , radius: Radius e.radius
      , sale: Nothing
      }
  , signalForSale: \(SignalMarket.SignalForSale e) -> Nothing
  , signalUnlisted: \(SignalMarket.SignalUnlisted e) -> Nothing
  , signalSold: \(SignalMarket.SignalSold e) -> Nothing
  }


eventToSignalUpdate :: Event -> Signal -> Maybe Signal
eventToSignalUpdate = _.raw >>> V.match
  { transfer: \(FoamToken.Transfer e) -> const Nothing
  , trackedToken: \(SignalToken.TrackedToken e) -> const Nothing
  , signalForSale: \(SignalMarket.SignalForSale e) -> \(Signal s) -> do
      guard $ s.id == SignalId e.tokenId
      pure $ Signal s { sale = Just {price: Token e.price, id: SaleId e.saleId} }
  , signalUnlisted: \(SignalMarket.SignalUnlisted e) -> \(Signal s) -> do
      guard $ s.id == SignalId e.tokenId
      pure $ Signal s { sale = Nothing }
  , signalSold: \(SignalMarket.SignalSold e) -> \(Signal s) -> do
      guard $ s.id == SignalId e.tokenId
      pure $ Signal s { owner = e.newOwner, sale = Nothing }
  }

eventToSignalActivity :: Event -> Maybe SignalActivity
eventToSignalActivity { raw } = raw # V.match
  { transfer: \(FoamToken.Transfer e) -> Nothing
  , trackedToken: \(SignalToken.TrackedToken e) -> Nothing
  , signalForSale: \(SignalMarket.SignalForSale e) ->
      Just $ ListedForSale { owner: e.seller, price: Token e.price, saleId: SaleId e.saleId }
  , signalUnlisted: \(SignalMarket.SignalUnlisted e) ->
      Just $ UnlistedFromSale { owner: e.owner, saleId: SaleId e.saleId }
  , signalSold: \(SignalMarket.SignalSold e) ->
      Just $ Sold { owner: e.owner, buyer: e.newOwner, saleId: SaleId e.saleId, price: Token e.price }
  }

eventToActivity :: forall m . Monad m => Event -> ReaderT (SignalId -> m Signal) m (Maybe Activity)
eventToActivity { raw } = raw # V.match
  { transfer: \(FoamToken.Transfer e) -> pure $ Just $ TokenTransfer {from:e.from, to: e.to, amount: Token e.value}
  , trackedToken: \(SignalToken.TrackedToken e) -> pure Nothing
  , signalForSale: \(SignalMarket.SignalForSale e) -> ask >>= \getter -> do
      signal <- lift $ getter $ SignalId e.tokenId
      pure $ Just $ SignalListedForSale { owner: e.seller, price: Token e.price, saleId: SaleId e.saleId, signal }
  , signalUnlisted: \(SignalMarket.SignalUnlisted e) -> ask >>= \getter -> do
      signal <- lift $ getter $ SignalId e.tokenId
      pure $ Just $ SignalUnlistedFromSale { owner: e.owner, saleId: SaleId e.saleId, signal }
  , signalSold: \(SignalMarket.SignalSold e) -> ask >>= \getter -> do
      signal <- lift $ getter $ SignalId e.tokenId
      pure $ Just $ SignalSold { owner: e.owner, buyer: e.newOwner, saleId: SaleId e.saleId, price: Token e.price, signal }
  }
