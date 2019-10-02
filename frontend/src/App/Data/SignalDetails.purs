module App.Data.SignalDetails where

import App.Data.Signal (Signal)
import App.Data.SignalActivity (SignalActivity)

newtype SignalDetails = SignalDetails
  { signal :: Signal
  , activity :: Array SignalActivity
  }
