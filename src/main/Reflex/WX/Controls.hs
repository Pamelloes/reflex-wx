{-|
Module      : Reflex.WX.Controls
Description : This module contains wrappers for the functions in
              Graphics.UI.WX.Controls.
License     : wxWindows Library License
Maintainer  : joshuabrot@gmail.com
Stability   : Experimental
-}
{-# LANGUAGE RankNTypes #-}
module Reflex.WX.Controls ( panel
                          , notebook
                          , button
                          , smallButton
                          , bitmapButton
                          , entry
                          , textEntry
                          , textCtrl
                          , textCtrlRich
                          , checkBox
                          , choice
                          , comboBox
                          , singleListBox
                          , multiListBox
                          , toggleButton
                          , bitmapToggleButton
                          , treeCtrl
                          , listCtrl
                          , staticText
                          , splitterWindow
                          , mediaCtrl
                          , styledTextCtrl
                          , propertyGrid
                          ) where

import Reflex.WX.Class

import qualified Graphics.UI.WX.Controls as C
import Graphics.UI.WX.Layout
import Graphics.UI.WX.Window

--type Component w m = Window w -> m Layout

tocomp :: (Functor m, Widget b) => (Window w -> a -> m b) -> a -> Component w m
tocomp = fmap (fmap (fmap widget)) . flip

panel    = tocomp C.panel
notebook = tocomp C.notebook

button       = tocomp C.button
smallButton  = tocomp C.smallButton
bitmapButton = tocomp C.bitmapButton

entry        = tocomp C.entry
textEntry    = tocomp C.textEntry
textCtrl     = tocomp C.textCtrl
textCtrlRich = tocomp C.textCtrlRich

checkBox = tocomp C.checkBox

choice = tocomp C.choice

comboBox = tocomp C.comboBox

singleListBox = tocomp C.singleListBox
multiListBox  = tocomp C.multiListBox

toggleButton       = tocomp C.toggleButton
bitmapToggleButton = tocomp C.bitmapToggleButton

treeCtrl = tocomp C.treeCtrl

listCtrl = tocomp C.listCtrl

staticText = tocomp C.staticText

splitterWindow = tocomp C.splitterWindow

mediaCtrl = tocomp C.mediaCtrl

styledTextCtrl = tocomp C.styledTextCtrl

propertyGrid = tocomp C.propertyGrid
