{-|
Module      : Reflex.WX.Layout
Description : This module re-exports appropriate functions from
              Graphics.UI.WXCore.Layout and provides the necessary instance
              declaration to make the functions work.
License     : wxWindows Library License
Maintainer  : joshuabrot@gmail.com
Stability   : Experimental
-}
module Reflex.WX.Layout ( W.Layout

                        , W.widget

                        , W.rule
                        , W.hrule
                        , W.vrule

                        , W.row
                        , W.column
                        , W.grid
                        , W.boxed

                        , W.glue
                        , W.hglue
                        , W.vglue

                        , W.space
                        , W.hspace
                        , W.vspace
                        , W.empty

                        , W.dynamic

                        , W.static
                        , W.stretch
                        , W.hstretch
                        , W.vstretch
                        , W.minsize

                        , W.rigid
                        , W.shaped 
                        , W.expand
                        
                        , W.fill
                        , W.hfill
                        , W.vfill

                        , W.margin
                        , W.marginWidth
                        , W.marginNone
                        , W.marginLeft
                        , W.marginTop
                        , W.marginRight
                        , W.marginBottom

                        , W.floatTopLeft
                        , W.floatTop
                        , W.floatTopRight
                        , W.floatLeft
                        , W.floatCentre
                        , W.floatCenter
                        , W.floatRight
                        , W.floatBottomLeft
                        , W.floatBottom
                        , W.floatBottomRight

                        , W.hfloatLeft
                        , W.hfloatCentre
                        , W.hfloatCenter
                        , W.hfloatRight

                        , W.vfloatTop
                        , W.vfloatCentre
                        , W.vfloatCenter
                        , W.vfloatBottom

                        , W.alignTopLeft
                        , W.alignTop
                        , W.alignTopRight
                        , W.alignLeft
                        , W.alignCentre
                        , W.alignCenter
                        , W.alignRight
                        , W.alignBottomLeft
                        , W.alignBottom
                        , W.alignBottomRight

                        , W.halignLeft
                        , W.halignCentre
                        , W.halignCenter
                        , W.halignRight

                        , W.valignTop
                        , W.valignCentre
                        , W.valignCenter
                        , W.valignBottom
                        ) where

import qualified Graphics.UI.WXCore.Layout as W

import Reflex.WX.Class

instance W.Widget w => W.Widget (Widget t w) where
  widget w = W.widget (wxwidget w)
