{-|
Module      : Reflex.WX.Attributes
Description : This module contains the definitions for attributes common to many
              different controls.
License     : wxWindows Library License
Maintainer  : joshuabrot@gmail.com
Stability   : Experimental
-}
{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
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
                            , Items (..)
                            , Able (..)
                            , Help (..)
                            , Tipped (..)
                            , Styled (..)
                            , Framed (..)
                            , Checkable (..)
                            , Pictured (..)
                            , Sized (..)
                            ) where

import qualified Graphics.UI.WX as W

import Reflex
import Reflex.WX.Class

class Selecting t w where
  select :: Event t ()

class Commanding t w where
  command :: Event t ()

class Updating t w where
  update :: Event t ()

class Reactive t w where
  mouse    :: Event t W.EventMouse
  keyboard :: Event t W.EventKey
  closing  :: Event t ()
  resize   :: Event t ()
  focus    :: Event t Bool
  activate :: Event t Bool

-- TODO add reactive-based events

class Textual t w where
  text :: Attr t w String

class Literate t w where
  font          :: Attr t w W.FontStyle
  fontSize      :: Attr t w Int
  fontWeight    :: Attr t w W.FontWeight
  fontFamily    :: Attr t w W.FontFamily
  fontShape     :: Attr t w W.FontShape
  fontFace      :: Attr t w String
  fontUnderline :: Attr t w Bool
  textColor     :: Attr t w W.Color
  textBgcolor   :: Attr t w W.Color

class Dimensions t w where
  outerSize   :: Attr t w W.Size
  position    :: Attr t w W.Point
  area        :: Attr t w W.Rect
  bestSize    :: Attr t w W.Size -- Read only
  clientSize  :: Attr t w W.Size
  virtualSize :: Attr t w W.Size

class Colored t w where
  bgcolor :: Attr t w W.Color
  color   :: Attr t w W.Color

class Visible t w where
  visible :: Attr t w Bool

class Bordered t w where
  border :: Attr t w W.Border

class Selection t w where
  selection :: Attr t w Int

class Selections t w where
  selections :: Attr t w [Int]

-- TODO Do some magic to work with dynamics
class Items t w a | w -> a where
  itemCount :: Attr t w Int -- Read only
  items     :: Attr t w [a]
  item      :: Int -> Attr t w a

class Able t w where
  enabled :: Attr t w Bool

class Help t w where
  help :: Attr t w String

class Tipped t w where
  tooltip :: Attr t w String

class Styled t w where
  style :: Attr t w Int -- TODO figure out how to gracefully handle styles

-- These attributes won't update after creation. Encode somehow?
class Framed t w where
  resizeable   :: Attr t w Bool
  minimizeable :: Attr t w Bool
  maximizable  :: Attr t w Bool
  closeable    :: Attr t w Bool

class Checkable t w where
  checkable :: Attr t w Bool
  checked   :: Attr t w Bool

class Pictured t w where
  picture :: Attr t w FilePath

class Sized t w where
  size :: Attr t w W.Size

{-
-- TODO work out types, add additional defaults
class HasDefault t w where
  defaultButton :: Attr t w (Button ())
-}
