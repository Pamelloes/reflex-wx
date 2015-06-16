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
import Reflex.WX.Layout

wrapWC :: (W.Widget w, MonadWidget t m) => 
          (forall a. W.Window a -> [W.Prop w] -> IO(w)) -> [Prop t w] 
            -> m (Widget t w)
wrapWC f p = do 
  (AW w) <- askParent
  rec prop <- sequence $ fmap (unwrapProp x) p
      x    <- liftIO $ f w prop
  return $ Widget x p

wrapWF :: forall w t m b. (W.Form (W.Window w), MonadWidget t m) => 
          (forall a. W.Window a -> [W.Prop (W.Window w)] -> IO (W.Window w))
            -> [Prop t (W.Window w)] -> m Layout -> m (Widget t (W.Window w))
wrapWF f p l = do 
  (AW w) <- askParent
  rec prop <- sequence $ fmap (unwrapProp x) p
      x    <- liftIO $ f w prop

  pushParent (AW x)
  rl <- l
  popParent
  liftIO $ W.set x [W.layout W.:= rl]

  return $ Widget x p

wrapWT :: MonadWidget t m => 
          ([W.Prop (W.TopLevelWindow w)] -> IO (W.TopLevelWindow w))
            -> [Prop t (W.TopLevelWindow w)] -> m Layout 
              -> m (Widget t (W.TopLevelWindow w))
wrapWT f p l = do
  rec prop <- sequence $ fmap (unwrapProp x) p
      x    <- liftIO $ f prop

  pushParent (AW x)
  rl <- l
  popParent
  liftIO $ W.set x [W.layout W.:= rl]

  return $ Widget x p

-- Window
type Window t a = Widget t (W.Window a)

window :: (MonadWidget t m) => 
          [Prop t (W.Window ())] -> m (Window t ())
window = wrapWC W.window

tabTraversal :: Attr t (W.Window a) Bool
tabTraversal = wrapAttr W.tabTraversal

instance Able t (Widget t) (W.Window a) where
  enabled = wrapAttr W.enabled
instance Bordered t (Widget t) (W.Window a) where
  border = wrapAttr W.border
instance Colored t (Widget t) (W.Window a) where
  bgcolor = wrapAttr W.bgcolor
  color   = wrapAttr W.color
instance Dimensions t (Widget t) (W.Window a) where
  outerSize   = wrapAttr W.outerSize
  position    = wrapAttr W.position
  area        = wrapAttr W.area
  bestSize    = wrapAttr W.bestSize
  clientSize  = wrapAttr W.clientSize
  virtualSize = wrapAttr W.virtualSize
instance Literate t (Widget t) (W.Window a) where
  font = wrapAttr W.font
  fontSize      = wrapAttr W.fontSize
  fontWeight    = wrapAttr W.fontWeight
  fontFamily    = wrapAttr W.fontFamily
  fontShape     = wrapAttr W.fontShape
  fontFace      = wrapAttr W.fontFace
  fontUnderline = wrapAttr W.fontUnderline
  textColor     = wrapAttr W.textColor
  textBgcolor   = wrapAttr W.textBgcolor
instance Sized t (Widget t) (W.Window a) where
  size = wrapAttr W.size
instance Styled t (Widget t) (W.Window a) where
  style = wrapAttr W.style
instance Typeable a => Textual t (Widget t) (W.Window a) where
  text = Attr f W.text
    where f :: forall t m a. (Typeable a, MonadWidget t m) =>
               Widget t (W.Window a) -> m (Dynamic t String)
          f a = case ((gcast a)::Maybe (Widget t (W.TextCtrl ()))) of
                 Nothing -> (dget W.text) a
                 Just w  -> do
                   let get :: IO () -> String -> IO ()
                       get a _ = a
                   let set :: IO () -> (String -> IO ()) -> IO ()
                       set _ n = do
                                   str <- W.get (wxwidget w) W.text
                                   n str
                   let ev = W.mapEvent get set W.update
                   e <- wrapEvent1 ev w
                   holdDyn "" e
instance Tipped t (Widget t) (W.Window a) where
  tooltip = wrapAttr W.tooltip
instance Visible t (Widget t) (W.Window a) where
  visible = wrapAttr W.visible

instance Typeable a => Reactive t (Window t a) where
  mouse    = wrapEvent1 W.mouse
  keyboard = wrapEvent1 W.keyboard
  closing  = wrapEvent  W.closing
  resize   = wrapEvent  W.resize
  focus    = wrapEvent1 W.focus
  activate = wrapEvent1 W.activate

-- TopLevelWindow
type TopLevelWindow t a = Widget t (W.TopLevelWindow a)

-- TODO Closeable?
-- TODO Form?
instance Framed t (Widget t) (W.TopLevelWindow a) where
  resizeable   = wrapAttr W.resizeable
  minimizeable = wrapAttr W.minimizeable
  maximizeable = wrapAttr W.maximizeable
  closeable    = wrapAttr W.closeable
-- TODO HasDefault?
instance Pictured t (Widget t) (W.TopLevelWindow a) where
  picture = wrapAttr W.picture

-- Frame
type Frame t a = Widget t (W.Frame a)

frame :: (MonadWidget t m) => 
         [Prop t (W.Frame ())] -> m Layout -> m (Frame t ())
frame = wrapWT W.frame

-- Panel
type Panel t a = Widget t (W.Panel a)

panel :: (MonadWidget t m) => 
         [Prop t (W.Panel ())] -> m Layout -> m (Panel t ())
panel = wrapWF W.panel

--TODO Form?

-- ScrolledWindow
type ScrolledWindow t a = Widget t (W.ScrolledWindow a)

scrolledWindow :: (MonadWidget t m) => 
                  [Prop t (W.ScrolledWindow ())] -> m Layout
                    -> m (ScrolledWindow t ())
scrolledWindow = wrapWF W.scrolledWindow

scrollRate :: Attr t (W.ScrolledWindow a) Size
scrollRate = wrapAttr W.scrollRate

-- Button
type Button t a = Widget t (W.Button a)

button :: (MonadWidget t m) => 
          [Prop t (W.Button ())] -> m (Button t ())
button = wrapWC W.button

smallButton :: (MonadWidget t m) => 
               [Prop t (W.Button ())] -> m (Button t ())
smallButton = wrapWC W.smallButton

instance Typeable a => Commanding t (Button t a) where
  command = wrapEvent W.command

type BitmapButton t a = Widget t (W.BitmapButton a)

bitmapButton :: (MonadWidget t m) => 
                [Prop t (W.BitmapButton ())] -> m (BitmapButton t ())
bitmapButton = wrapWC W.bitmapButton

instance Pictured t (Widget t) (W.BitmapButton a) where
  picture = wrapAttr W.picture

-- TextCtrl
type TextCtrl t a = Widget t (W.TextCtrl a)

entry :: (MonadWidget t m) => 
         [Prop t (W.TextCtrl ())] -> m (TextCtrl t ())
entry = textEntry

textEntry :: (MonadWidget t m) => 
             [Prop t (W.TextCtrl ())] -> m (TextCtrl t ())
textEntry = wrapWC W.textEntry

textCtrl :: (MonadWidget t m) => 
            [Prop t (W.TextCtrl ())] -> m (TextCtrl t ())
textCtrl = wrapWC W.textCtrl

processEnter :: Attr t (W.TextCtrl a) Bool
processEnter = wrapAttr W.processEnter

processTab :: Attr t (W.TextCtrl a) Bool
processTab = wrapAttr W.processTab

instance Aligned t (Widget t) (W.TextCtrl a) where
  alignment = wrapAttr W.alignment
instance Wrapped t (Widget t) (W.TextCtrl a) where
  wrap = wrapAttr W.wrap
instance Typeable a => Commanding t (TextCtrl t a) where
  command = wrapEvent W.command
instance Typeable a => Updating t (TextCtrl t a) where
  update c = do
               t <- get text c
               return $ fmap (const ()) (updated t)

-- StaticText
type StaticText t a = Widget t (W.StaticText a)

staticText :: (MonadWidget t m) => 
              [Prop t (W.StaticText ())] -> m (StaticText t ())
staticText = wrapWC W.staticText

type Label t a = StaticText t a

label :: (MonadWidget t m) => 
         [Prop t (W.StaticText ())] -> m (Label t ())
label = staticText

-- CheckBox
type CheckBox t a = Widget t (W.CheckBox a)

checkBox :: (MonadWidget t m) => 
            [Prop t (W.CheckBox ())] -> m (CheckBox t ())
checkBox = wrapWC W.checkBox

instance Typeable a => Commanding t (CheckBox t a) where
  command = wrapEvent W.command
instance Checkable t (Widget t) (W.CheckBox a) where
  checkable = wrapAttr W.checkable
  checked   = wrapAttr W.checked

-- Choice
type Choice t a = Widget t (W.Choice a)

choice :: (MonadWidget t m) => 
          [Prop t (W.Choice ())] -> m (Choice t ())
choice = wrapWC W.choice

instance Sorted t (Widget t) (W.Choice a) where
  sorted = wrapAttr W.sorted
instance Selecting t (Choice t ()) where
  select = wrapEvent W.select
instance Selection t (Widget t) (W.Choice ()) where
  selection = wrapAttr W.selection
-- TODO Items
