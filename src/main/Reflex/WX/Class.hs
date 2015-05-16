{-|
Module      : Reflex.WX.Class
Description : This module contains the basic type definitions used within a
              reflex-wx program.
License     : wxWindows Library License
Maintainer  : joshuabrot@gmail.com
Stability   : Experimental
-}
{-# LANGUAGE ExistentialQuantification, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module Reflex.WX.Class ( Prop (..)
--                       , get
                       , towp
                       , Component
                       , ComponentM
                       , addVoidAction
                       , registerProp
                       ) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.State

import Reflex

import qualified Graphics.UI.WX as W

data Prop t w = forall a. W.Attr w a := a
              | forall a. W.Attr w a :~ Dynamic t a

towp :: (Reflex t, MonadSample t m, MonadIO m, Monad m) => w -> Prop t w
        -> ComponentM t m (W.Prop w)
towp w (a := v) = return $ a W.:= v
towp w (a :~ v) = do
  addVoidAction $ fmap (\x -> liftIO $ W.set w [a W.:= x]) (updated v)
  cv <- sample $ current v
  return $ a W.:= cv

--get :: w -> W.Attr w a -> Behavior t a
--get = undefined

type Component w m = W.Window w -> m W.Layout

data ComponentState t m = ComponentState {
  voidActions :: [Event t (m ())]
}

type ComponentInternal t m a = StateT (ComponentState t m) m a

newtype ComponentM t m a = ComponentM { unCM :: ComponentInternal t m a}
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO)
instance MonadSample t m => MonadSample t (ComponentM t m) where
  sample = ComponentM . lift . sample

addVoidAction ::  Monad m => Event t (m ()) -> ComponentM t m ()
addVoidAction e = ComponentM $ modify (\(ComponentState a)->ComponentState (e:a))

registerProp :: (Reflex t, Monad m, MonadIO m, MonadSample t m) => [Prop t w] 
                 -> w -> ComponentM t m ()
registerProp p w = do
  props <- sequence $ fmap (towp w) p
  liftIO $ W.set w props
  return ()
