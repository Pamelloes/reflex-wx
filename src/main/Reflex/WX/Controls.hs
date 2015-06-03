{-|
Module      : Reflex.WX.Controls
Description : This module contains wrappers for the functions in
              Graphics.UI.WX.Controls.
License     : wxWindows Library License
Maintainer  : joshuabrot@gmail.com
Stability   : Experimental
-}
{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, RecursiveDo, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances #-}
module Reflex.WX.Controls ( Window
                          , window
                          , tabTraversal

                          , TopLevelWindow

                          , Frame
                          , frame

                          , Panel
                          , panel
                          
                          , ScrolledWindow
                          , scrolledWindow
                          , scrollRate

                          , Button
                          , button
                          , smallButton
                          , BitmapButton
                          , bitmapButton

                          , TextCtrl
                          , entry
                          , textEntry
                          , textCtrl
                          , processEnter
                          , processTab

                          , StaticText
                          , staticText
                          , Label
                          , label

                          , CheckBox
                          , checkBox

                          , Choice
                          , choice
                          ) where

import Control.Monad.Fix
import Control.Monad.IO.Class

import Data.Typeable

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
instance Typeable a => Textual t (Component t) (W.Window a) where
  text = Attr f W.text
    where f :: forall t m a. (Typeable a, MonadComponent t m) =>
               Component t (W.Window a) -> m (Dynamic t String)
          f a = case ((gcast a)::Maybe (Component t (W.TextCtrl ()))) of
                 Nothing -> (dget W.text) a
                 Just v  -> do
                   let Component (w,p) = v
                   let get :: IO () -> String -> IO ()
                       get a _ = a
                   let set :: IO () -> (String -> IO ()) -> IO ()
                       set _ n = do
                                   str <- W.get w W.text
                                   n str
                   let ev = W.mapEvent get set W.update
                   e <- wrapEvent1 ev v
                   holdDyn "" e
instance Tipped t (Component t) (W.Window a) where
  tooltip = wrapAttr W.tooltip
instance Visible t (Component t) (W.Window a) where
  visible = wrapAttr W.visible

instance Typeable a => Reactive t (Window t a) where
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

--TODO Form?

-- ScrolledWindow
type ScrolledWindow t a = Component t (W.ScrolledWindow a)

scrolledWindow :: (MonadComponent t m) => 
                  [Prop t (W.ScrolledWindow ())] -> m a 
                    -> m (ScrolledWindow t (),a)
scrolledWindow = wrapWF W.scrolledWindow

scrollRate :: Attr t (W.ScrolledWindow a) Size
scrollRate = wrapAttr W.scrollRate

-- Button
type Button t a = Component t (W.Button a)

button :: (MonadComponent t m) => 
          [Prop t (W.Button ())] -> m (Button t ())
button = wrapWC W.button

smallButton :: (MonadComponent t m) => 
               [Prop t (W.Button ())] -> m (Button t ())
smallButton = wrapWC W.smallButton

instance Typeable a => Commanding t (Button t a) where
  command = wrapEvent W.command

type BitmapButton t a = Component t (W.BitmapButton a)

bitmapButton :: (MonadComponent t m) => 
                [Prop t (W.BitmapButton ())] -> m (BitmapButton t ())
bitmapButton = wrapWC W.bitmapButton

instance Pictured t (Component t) (W.BitmapButton a) where
  picture = wrapAttr W.picture

-- TextCtrl
type TextCtrl t a = Component t (W.TextCtrl a)

entry :: (MonadComponent t m) => 
         [Prop t (W.TextCtrl ())] -> m (TextCtrl t ())
entry = textEntry

textEntry :: (MonadComponent t m) => 
             [Prop t (W.TextCtrl ())] -> m (TextCtrl t ())
textEntry = wrapWC W.textEntry

textCtrl :: (MonadComponent t m) => 
            [Prop t (W.TextCtrl ())] -> m (TextCtrl t ())
textCtrl = wrapWC W.textCtrl

processEnter :: Attr t (W.TextCtrl a) Bool
processEnter = wrapAttr W.processEnter

processTab :: Attr t (W.TextCtrl a) Bool
processTab = wrapAttr W.processTab

instance Aligned t (Component t) (W.TextCtrl a) where
  alignment = wrapAttr W.alignment
instance Wrapped t (Component t) (W.TextCtrl a) where
  wrap = wrapAttr W.wrap
instance Typeable a => Commanding t (TextCtrl t a) where
  command = wrapEvent W.command
instance Typeable a => Updating t (TextCtrl t a) where
  update c = do
               t <- get text c
               return $ fmap (const ()) (updated t)

-- StaticText
type StaticText t a = Component t (W.StaticText a)

staticText :: (MonadComponent t m) => 
              [Prop t (W.StaticText ())] -> m (StaticText t ())
staticText = wrapWC W.staticText

type Label t a = StaticText t a

label :: (MonadComponent t m) => 
         [Prop t (W.StaticText ())] -> m (Label t ())
label = staticText

-- CheckBox
type CheckBox t a = Component t (W.CheckBox a)

checkBox :: (MonadComponent t m) => 
            [Prop t (W.CheckBox ())] -> m (CheckBox t ())
checkBox = wrapWC W.checkBox

instance Typeable a => Commanding t (CheckBox t a) where
  command = wrapEvent W.command
instance Checkable t (Component t) (W.CheckBox a) where
  checkable = wrapAttr W.checkable
  checked   = wrapAttr W.checked

-- Choice
type Choice t a = Component t (W.Choice a)

choice :: (MonadComponent t m) => 
          [Prop t (W.Choice ())] -> m (Choice t ())
choice = wrapWC W.choice

instance Sorted t (Component t) (W.Choice a) where
  sorted = wrapAttr W.sorted
instance Selecting t (Choice t ()) where
  select = wrapEvent W.select
instance Selection t (Component t) (W.Choice ()) where
  selection = wrapAttr W.selection
-- TODO Items
