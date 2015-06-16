{-|
Module      : Reflex.WX
Description : The root module for reflex-wx. This will import all necessary
              modules for normal use of reflex-wx.
License     : wxWindows Library License
Maintainer  : joshuabrot@gmail.com
Stability   : Experimental
-}
module Reflex.WX ( module Reflex
                 , module Reflex.WX.Class
                 , module Reflex.WX.Attributes
                 , module Reflex.WX.Controls
                 , module Reflex.WX.Internal
                 , module Reflex.WX.Layout
                 ) where

import Reflex hiding (select) -- Alternatively, rename Attributes.select
import Reflex.WX.Class
import Reflex.WX.Attributes
import Reflex.WX.Controls
import Reflex.WX.Internal
import Reflex.WX.Layout
