{-|
Module      : Reflex.WX.Controls
Description : This module contains wrappers for the functions in
              Graphics.UI.WX.Controls.
License     : wxWindows Library License
Maintainer  : joshuabrot@gmail.com
Stability   : Experimental
-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RecursiveDo #-}
{-# LANGUAGE RankNTypes, TypeSynonymInstances #-}
module Reflex.WX.Controls ( Window
                          , window
                          ) where

import Control.Monad.Fix
import Control.Monad.IO.Class

import qualified Graphics.UI.WX as W
import qualified Graphics.UI.WXCore as W

import Reflex
import Reflex.WX.Class
import Reflex.WX.Attributes

fromwc :: (W.Widget w, MonadComponent t m) => 
          (forall a. W.Window a -> [W.Prop w] -> IO(w)) -> [Prop t w] 
            -> m (Component t w)
fromwc f p = do 
  (AW w) <- askParent
  rec prop <- sequence $ fmap (towp x) p
      x    <- liftIO $ f w prop
  let c = Component (x,p)
  addComponent c
  return c

type Window t a = Component t (W.Window a)

window :: (MonadComponent t m) => 
          [Prop t (W.Window ())] -> m (Window t ())
window = fromwc W.window

tabTraversal :: Attr t (W.Window a) Bool
tabTraversal = fromwa W.tabTraversal

instance Able t (Component t) (W.Window a) where
  enabled = fromwa W.enabled

{-
frame :: (MonadComponent t m) => 
         [Prop t (W.Frame ())] -> m a -> m (Component t (W.Frame ()),a)
frame p c = do
  rec prop <- sequence $ fmap (towp x) p
      x    <- liftIO $ W.frame prop

  pushComponents (AW x)
  a <- c
  l <- popComponents
  liftIO $ W.set x [W.layout W.:= l]

  let cp = Component (x,p)
  --addComponent cp
  return (cp,a)


fromwf :: forall w t m b. (W.Form (W.Window w), MonadComponent t m) => 
          (forall a. W.Window a -> [W.Prop (W.Window w)] -> IO (W.Window w))
            -> [Prop t (W.Window w)] -> m b -> m (Component t (W.Window w),b)
fromwf f p c = do 
  (AW w) <- askParent
  rec prop <- sequence $ fmap (towp x) p
      x    <- liftIO $ f w prop

  pushComponents (AW x)
  a <- c
  l <- popComponents
  liftIO $ W.set x [W.layout W.:= l]

  let cp = Component (x,p)
  addComponent cp
  return (cp,a)
         

panel :: (MonadComponent t m) => 
         [Prop t (W.Panel ())] -> m a -> m (Component t (W.Panel ()),a)
panel = fromwf W.panel

button :: (MonadComponent t m) => 
          [Prop t (W.Button ())] -> m (Component t (W.Button ()))
button = fromwc W.button

staticText :: (MonadComponent t m) => 
          [Prop t (W.StaticText ())] -> m (Component t (W.StaticText ()))
staticText = fromwc W.staticText

{-
command :: (W.Commanding w, MonadComponent t m) => 
           Component t w -> m (Event t ())
command = wrapEvent W.command
-}
-}
