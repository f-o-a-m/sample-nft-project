module App.Data.SignalDetails where


import Prelude

import App.Data.Signal (Signal)
import App.Data.SignalActivity (SignalActivity)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)


newtype SignalDetails = SignalDetails
  { signal :: Signal
  , activity :: Array SignalActivity
  }

derive instance newtypeSignalDetails :: Newtype SignalDetails _
derive instance genericSignalDetails :: Generic SignalDetails _
instance eqSignalDetails :: Eq SignalDetails where eq = genericEq
instance ordSignalDetails :: Ord SignalDetails where compare = genericCompare
instance showSignalDetails :: Show SignalDetails where show = genericShow
