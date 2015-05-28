{-|
Module      : Reflex.WX.Controls
Description : This module contains wrappers for the functions in
              Graphics.UI.WX.Controls.
License     : wxWindows Library License
Maintainer  : joshuabrot@gmail.com
Stability   : Experimental
-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo, RankNTypes, TypeSynonymInstances #-}
module Reflex.WX.Controls ( Window
                          , window
                          , tabTraversal

                          , TopLevelWindow

                          , Frame
                          , frame

                          , Panel
                          , panel

                          , Button
                          , button

                          , StaticText
                          , staticText
                          , Label
                          , label
                          ) where

import Control.Monad.Fix
import Control.Monad.IO.Class

import qualified Graphics.UI.WX as W
import qualified Graphics.UI.WXCore as W

import Reflex
import Reflex.WX.Class
import Reflex.WX.Attributes

wrapWC :: (W.Widget w, MonadComponent t m) => 
          (forall a. W.Window a -> [W.Prop w] -> IO(w)) -> [Prop t w] 
            -> m (Component t w)
wrapWC f p = do 
  (AW w) <- askParent
  rec prop <- sequence $ fmap (unwrapProp x) p
      x    <- liftIO $ f w prop
  let c = Component (x,p)
  addComponent c
  return c

wrapWF :: forall w t m b. (W.Form (W.Window w), MonadComponent t m) => 
          (forall a. W.Window a -> [W.Prop (W.Window w)] -> IO (W.Window w))
            -> [Prop t (W.Window w)] -> m b -> m (Component t (W.Window w),b)
wrapWF f p c = do 
  (AW w) <- askParent
  rec prop <- sequence $ fmap (unwrapProp x) p
      x    <- liftIO $ f w prop

  pushComponents (AW x)
  a <- c
  l <- popComponents
  liftIO $ W.set x [W.layout W.:= l]

  let cp = Component (x,p)
  addComponent cp
  return (cp,a)

wrapWT :: MonadComponent t m => 
          ([W.Prop (W.TopLevelWindow w)] -> IO (W.TopLevelWindow w))
            -> [Prop t (W.TopLevelWindow w)] -> m b 
              -> m (Component t (W.TopLevelWindow w),b)
wrapWT f p c = do
  rec prop <- sequence $ fmap (unwrapProp x) p
      x    <- liftIO $ f prop

  pushComponents (AW x)
  a <- c
  l <- popComponents
  liftIO $ W.set x [W.layout W.:= l]

  let cp = Component (x,p)
  addComponent cp
  return (cp,a)

-- Window
type Window t a = Component t (W.Window a)

window :: (MonadComponent t m) => 
          [Prop t (W.Window ())] -> m (Window t ())
window = wrapWC W.window

tabTraversal :: Attr t (W.Window a) Bool
tabTraversal = wrapAttr W.tabTraversal

instance Able t (Component t) (W.Window a) where
  enabled = wrapAttr W.enabled
instance Bordered t (Component t) (W.Window a) where
  border = wrapAttr W.border
instance Colored t (Component t) (W.Window a) where
  bgcolor = wrapAttr W.bgcolor
  color   = wrapAttr W.color
instance Dimensions t (Component t) (W.Window a) where
  outerSize   = wrapAttr W.outerSize
  position    = wrapAttr W.position
  area        = wrapAttr W.area
  bestSize    = wrapAttr W.bestSize
  clientSize  = wrapAttr W.clientSize
  virtualSize = wrapAttr W.virtualSize
instance Literate t (Component t) (W.Window a) where
  font = wrapAttr W.font
  fontSize      = wrapAttr W.fontSize
  fontWeight    = wrapAttr W.fontWeight
  fontFamily    = wrapAttr W.fontFamily
  fontShape     = wrapAttr W.fontShape
  fontFace      = wrapAttr W.fontFace
  fontUnderline = wrapAttr W.fontUnderline
  textColor     = wrapAttr W.textColor
  textBgcolor   = wrapAttr W.textBgcolor
instance Sized t (Component t) (W.Window a) where
  size = wrapAttr W.size
instance Styled t (Component t) (W.Window a) where
  style = wrapAttr W.style
instance Textual t (Component t) (W.Window a) where
  text = wrapAttr W.text
instance Tipped t (Component t) (W.Window a) where
  tooltip = wrapAttr W.tooltip
instance Visible t (Component t) (W.Window a) where
  visible = wrapAttr W.visible

instance Reactive t (Window t a) where
  mouse    = wrapEvent1 W.mouse
  keyboard = wrapEvent1 W.keyboard
  closing  = wrapEvent  W.closing
  resize   = wrapEvent  W.resize
  focus    = wrapEvent1 W.focus
  activate = wrapEvent1 W.activate

-- TopLevelWindow
type TopLevelWindow t a = Component t (W.TopLevelWindow a)

-- TODO Closeable?
-- TODO Form?
instance Framed t (Component t) (W.TopLevelWindow a) where
  resizeable   = wrapAttr W.resizeable
  minimizeable = wrapAttr W.minimizeable
  maximizeable = wrapAttr W.maximizeable
  closeable    = wrapAttr W.closeable
-- TODO HasDefault?
instance Pictured t (Component t) (W.TopLevelWindow a) where
  picture = wrapAttr W.picture

-- Frame
type Frame t a = Component t (W.Frame a)

frame :: (MonadComponent t m) => 
         [Prop t (W.Frame ())] -> m a -> m (Frame t (),a)
frame = wrapWT W.frame

-- Panel
type Panel t a = Component t (W.Panel a)

panel :: (MonadComponent t m) => 
         [Prop t (W.Panel ())] -> m a -> m (Panel t (),a)
panel = wrapWF W.panel

-- Button
type Button t a = Component t (W.Button a)

button :: (MonadComponent t m) => 
          [Prop t (W.Button ())] -> m (Button t ())
button = wrapWC W.button

instance Commanding t (Button t a) where
  command = wrapEvent W.command

-- StaticText
type StaticText t a = Component t (W.StaticText a)

staticText :: (MonadComponent t m) => 
              [Prop t (W.StaticText ())] -> m (StaticText t ())
staticText = wrapWC W.staticText

type Label t a = StaticText t a

label :: (MonadComponent t m) => 
         [Prop t (W.StaticText ())] -> m (Label t ())
label = staticText
