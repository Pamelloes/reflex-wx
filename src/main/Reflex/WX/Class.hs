{-|
Module      : Reflex.WX.Class
Description : This module contains the basic type definitions used within a
              reflex-wx program.
License     : wxWindows Library License
Maintainer  : joshuabrot@gmail.com
Stability   : Experimental
-}
{-# LANGUAGE RankNTypes #-}
module Reflex.WX.Class ( Component
--                       , ComponentInternal
--                       , ComponentEnv
                       ) where

import Control.Monad.Reader

import Graphics.UI.WXCore.Layout
import Graphics.UI.WXCore.WxcClassTypes

type Component w m = Window w -> m Layout

