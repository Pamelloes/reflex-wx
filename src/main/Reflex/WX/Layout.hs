{-|
Module      : Reflex.WX.Layout
Description : This module contains a variety of functions used to layout
              components.
License     : wxWindows Library License
Maintainer  : joshuabrot@gmail.com
Stability   : Experimental
-}
module Reflex.WX.Layout (
                        ) where

import qualified Graphics.UI.WX as W

import Reflex.WX.Class

class Form a where
  askLayout       :: a -> W.Layout
  setLayout    :: a -> W.Layout -> a
  modifyLayout :: (W.Layout -> W.Layout) -> a -> a
  modifyLayout f a = setLayout a $ f (askLayout a)

instance Form (Component t w) where
  askLayout = layout
  setLayout (Component a b _) l = Component a b l
