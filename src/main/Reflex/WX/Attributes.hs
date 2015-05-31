{-|
Module      : Reflex.WX.Attributes
Description : This module contains the definitions for attributes common to many
              different controls.
License     : wxWindows Library License
Maintainer  : joshuabrot@gmail.com
Stability   : Experimental
-}
{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, RankNTypes  #-}
module Reflex.WX.Attributes ( Selecting (..)
                            , Commanding (..)
                            , Updating (..)
                            , Reactive (..)

                            , W.Modifiers (..)
                            , W.showModifiers
                            , W.noneDown
                            , W.justShift
                            , W.justAlt
                            , W.justControl
                            , W.justMeta
                            , W.isNoneDown
                            , W.isNoShiftAltControlDown

                            , W.EventMouse (..)
                            , W.showMouse
                            , W.mousePos
                            , W.mouseModifiers

                            , W.EventKey (..)
                            , W.Key (..)
                            , W.keyKey
                            , W.keyModifiers
                            , W.keyPos
                            , W.showKey
                            , W.showKeyModifiers

                            , Textual (..)
                            , Literate (..)

                            , W.FontStyle (..)
                            , W.FontWeight (..)
                            , W.FontFamily (..)
                            , W.FontShape (..)
                            , W.fontDefault
                            , W.fontSwiss
                            , W.fontSmall
                            , W.fontItalic
                            , W.fontFixed

                            , W.Color
                            , W.rgb
                            , W.colorRGB
                            , W.colorRed
                            , W.colorGreen
                            , W.colorBlue
                            , W.black
                            , W.darkgrey
                            , W.dimgrey
                            , W.lightgrey
                            , W.white
                            , W.red
                            , W.green
                            , W.blue
                            , W.cyan
                            , W.magenta
                            , W.yellow
                            , W.SystemColor (..)
                            , W.colorSystem

                            , Dimensions (..)

                            , W.Point
                            , W.Point2 (..)
                            , W.point
                            , W.pt
                            , W.pointFromSize
                            , W.pointZero
                            , W.pointNull
                            , W.pointMoveBySize
                            , W.pointAdd
                            , W.pointSub
                            , W.pointScale

                            , W.Size
                            , W.Size2D (..)
                            , W.sz
                            , W.sizeFromPoint
                            , W.sizeZero
                            , W.sizeNull
                            , W.sizeEncloses
                            , W.sizeMin
                            , W.sizeMax

                            , Colored (..)
                            , Visible (..)
                            , Bordered (..)
                            
                            , W.Border (..)

                            , Selection (..)
                            , Selections (..)
                            --, Items (..)
                            , Able (..)
                            , Help (..)
                            , Tipped (..)
                            , Styled (..)
                            , Framed (..)
                            , Checkable (..)
                            , Pictured (..)
                            , Sized (..)
                            ) where

import Data.Typeable

import qualified Graphics.UI.WX as W

import Reflex
import Reflex.WX.Class

class Selecting t c where
  select :: forall m. MonadComponent t m => c -> m (Event t ())

class Commanding t c where
  command :: forall m. MonadComponent t m => c -> m (Event t ())

class Updating t c where
  update :: forall m. MonadComponent t m => c -> m (Event t ())

class Reactive t c where
  mouse    :: forall m. MonadComponent t m => c -> m (Event t W.EventMouse)
  keyboard :: forall m. MonadComponent t m => c -> m (Event t W.EventKey)
  closing  :: forall m. MonadComponent t m => c -> m (Event t ())
  resize   :: forall m. MonadComponent t m => c -> m (Event t ())
  focus    :: forall m. MonadComponent t m => c -> m (Event t Bool)
  activate :: forall m. MonadComponent t m => c -> m (Event t Bool)

-- TODO add reactive-based events

-- Yes, we need the Typeable hack...
class Typeable w => Textual t c w where
  text :: AttrC t c w String

class Literate t c w where
  font          :: AttrC t c w W.FontStyle
  fontSize      :: AttrC t c w Int
  fontWeight    :: AttrC t c w W.FontWeight
  fontFamily    :: AttrC t c w W.FontFamily
  fontShape     :: AttrC t c w W.FontShape
  fontFace      :: AttrC t c w String
  fontUnderline :: AttrC t c w Bool
  textColor     :: AttrC t c w W.Color
  textBgcolor   :: AttrC t c w W.Color

class Dimensions t c w where
  outerSize   :: AttrC t c w W.Size
  position    :: AttrC t c w W.Point
  area        :: AttrC t c w W.Rect
  bestSize    :: AttrC t c w W.Size -- Read only
  clientSize  :: AttrC t c w W.Size
  virtualSize :: AttrC t c w W.Size

class Colored t c w where
  bgcolor :: AttrC t c w W.Color
  color   :: AttrC t c w W.Color

class Visible t c w where
  visible :: AttrC t c w Bool

class Bordered t c w where
  border :: AttrC t c w W.Border

class Selection t c w where
  selection :: AttrC t c w Int

class Selections t c w where
  selections :: AttrC t c w [Int]

{- TODO Do some magic to work with dynamics
class Items t c w a | (w -> c, c w -> a) where
  itemCount :: Attr t w Int -- Read only
  items     :: Attr t w [a]
  item      :: Int -> Attr t w a
-}

class Able t c w where
  enabled :: AttrC t c w Bool

class Help t c w where
  help :: AttrC t c w String

class Tipped t c w where
  tooltip :: AttrC t c w String

class Styled t c w where
  style :: AttrC t c w Int -- TODO figure out how to gracefully handle styles

-- These attributes won't update after creation. Encode somehow?
class Framed t c w where
  resizeable   :: AttrC t c w Bool
  minimizeable :: AttrC t c w Bool
  maximizeable  :: AttrC t c w Bool
  closeable    :: AttrC t c w Bool

class Checkable t c w where
  checkable :: AttrC t c w Bool
  checked   :: AttrC t c w Bool

class Pictured t c w where
  picture :: AttrC t c w FilePath

class Sized t c w where
  size :: AttrC t c w W.Size

{-
-- TODO work out types, add additional defaults
class HasDefault t c where
  defaultButton :: AttrC t c (Button ())
-}
