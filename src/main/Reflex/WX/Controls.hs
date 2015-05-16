{-|
Module      : Reflex.WX.Controls
Description : This module contains wrappers for the functions in
              Graphics.UI.WX.Controls.
License     : wxWindows Library License
Maintainer  : joshuabrot@gmail.com
Stability   : Experimental
-}
{-# LANGUAGE MultiParamTypeClasses, RecursiveDo, RankNTypes #-}
module Reflex.WX.Controls ( frame
                          , button
                          , staticText
                          ) where

import Control.Monad.Fix
import Control.Monad.IO.Class

import qualified Graphics.UI.WX as W
import qualified Graphics.UI.WXCore as W

import Reflex
import Reflex.WX.Class

--type Component w m = Window w -> m Layout

fromwc :: (W.Widget w, Reflex t, Monad m, MonadIO m, MonadFix m, MonadSample t m) => 
          (W.Window a -> [W.Prop w] -> IO(w)) -> [Prop t w] 
            -> Component a (ComponentM t m)
fromwc f p w = do rec prop <- sequence $ fmap (towp x) p
                      x <- liftIO $ f w prop
                  return (W.widget x)

frame :: [Prop t (W.Frame ())] -> Component (W.CFrame ()) m -> Component w (ComponentM t m)
frame = undefined

button :: (Reflex t, Monad m, MonadIO m, MonadFix m, MonadSample t m) => 
          [Prop t (W.Button ())] -> Component w (ComponentM t m)
button = fromwc W.button

staticText :: (Reflex t, Monad m, MonadIO m, MonadFix m, MonadSample t m) => 
              [Prop t (W.StaticText())] -> Component w (ComponentM t m)
staticText = fromwc W.staticText
