{-|
Module      : Main
Description : An example program using reflex-wx
License     : wxWindows Library License
Maintainer  : joshuabrot@gmail.com
Stability   : Experimental
-}
{-# LANGUAGE RankNTypes, ImpredicativeTypes #-}
module Main where

import Reflex.WX

import Graphics.UI.WXCore
import Graphics.UI.WX (set, frame, start, text, layout, Prop (..))

crow :: forall w.Int -> [Component w IO] -> Component w IO
crow i = fmap (fmap (row i).sequence).sequence

content = crow 10 $
  [ button [ text := "Ok" ]
  , button [ text := "Cancel" ]
  , button [ text := "Swag" ]
  , staticText [ text := "Bitchez" ]
  ]

render :: Component (CTopLevelWindow (CFrame ())) IO -> IO ()
render c = start $ do
  f <- frame [text := "test"]
  l <- c f
  set f [ layout := l ]
  return ()
--  set f [layout := widget ch]

main :: IO ()
main = render content
