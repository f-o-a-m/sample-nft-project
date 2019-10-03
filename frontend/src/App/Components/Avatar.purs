module App.Components.Avatar where

import Prelude

import Data.Foldable (traverse_)
import Data.Int (hexadecimal)
import Data.Maybe (Maybe(..))
import Data.Number.Extras (integerFromStringAs)
import Data.String.CodePoints as Str
import Jazzicon as Jazzicon
import Network.Ethereum.Web3 (Address, unAddress, unHex)
import Partial.Unsafe (unsafeCrashWith)
import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM (css, findDOMNode)
import React.Basic.DOM as R
import Unsafe (unsafeFromRight)
import Web.DOM.Element as Element
import Web.DOM.Node (appendChild, removeChild)
import Web.DOM.Node as Node
import Web.DOM.NodeList as NodeList


type Props = Address

component :: React.Component Props
component = React.createComponent "Avatar"

avatar :: Props -> JSX
avatar = React.make component
  { initialState: unit
  , shouldUpdate: \self { nextProps } -> not $ nextProps == self.props
  , didUpdate: \self _ -> setIcon self
  , didMount: \self -> setIcon self
  , render: \self -> R.div {className: "Avatar", title: show self.props, style: css {height: "20px", width: "20px"}}
  }
  where
    setIcon self = do
      res <- findDOMNode self.instance_
      let node = unsafeFromRight "findDOMNode must be ok in didMount" res
      -- remove all children if any
      Node.childNodes node
        >>= NodeList.toArray
        >>= traverse_ (_ `removeChild` node)
      img <- Jazzicon.generate {dimension: 20, seed: addressToIconSeed self.props }
      void $ appendChild (Element.toNode img) node


    addressToIconSeed :: Address -> Jazzicon.Seed
    addressToIconSeed address =
      -- The seed calculation is ported from:
      -- https://github.com/MetaMask/metamask-extension/blob/v4.8.0/ui/lib/icon-factory.js#L60-L63
      case integerFromStringAs hexadecimal $ Str.take 8 $ unHex $ unAddress address of
        Nothing -> unsafeCrashWith $ "Failed to determine avatar seed for address: " <> show address
        Just seed -> Jazzicon.Seed seed
