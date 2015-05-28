{-|
Module      : Reflex.WX.Class
Description : This module contains the basic type definitions used within a
              reflex-wx program.
License     : wxWindows Library License
Maintainer  : joshuabrot@gmail.com
Stability   : Experimental
-}
{-# LANGUAGE ExistentialQuantification, FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes, MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Reflex.WX.Class ( AttrC (..)
                       , Attr
                       , PropC (..)
                       , Prop
                       , Component (..)
                       , AnyWindow (..)
                       , MonadComponent (..)
                       , find
                       , dget
                       , fromwa
                       , towp
                       , wrapEvent
                       , wrapEvent1
                       ) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.State hiding (get)

import Data.Dependent.Sum
import qualified Data.Dynamic as D
import Data.Typeable

import qualified Graphics.UI.WX as W

import Reflex
import Reflex.Host.Class hiding (fireEvents)

infixr 0 :=,:~

data AttrC t c w a = Attr {
  get :: forall m. MonadComponent t m => c w -> m (Dynamic t a),
  at  :: W.Attr w a
}
data PropC t c w = forall a. Typeable a => AttrC t c w a := a
                 | forall a. Typeable a => AttrC t c w a :~ Dynamic t a
type Attr t w a = AttrC t (Component t) w a
type Prop t w = PropC t (Component t) w

newtype Component t w = Component (w,[Prop t w])

data AnyWindow = forall w. AW (W.Window w)

class (Typeable t, Reflex t, MonadIO m, MonadHold t m
      ,MonadReflexCreateTrigger t m, MonadFix m
      ) => MonadComponent t m | m -> t where
  askParent       :: m AnyWindow
  addIOEvent      :: Event t (IO ()) -> m ()

  pushComponents  ::  AnyWindow -> m ()
  setLayout       :: ([W.Layout] -> W.Layout) -> m ()
  addComponent    :: (W.Widget w) => Component t w -> m ()
  popComponents   :: m (W.Layout)

  fireEvents      :: (a -> [DSum (EventTrigger t)]) -> m (a -> IO ())

find :: (Typeable a, Typeable t, Reflex t) =>
         W.Attr w a -> [Prop t w] -> Maybe (Either a (Dynamic t a))
find _ [] = Nothing
find a ((b := x):r)
  | n == m = case D.fromDynamic (D.toDyn x) of
               Just x  -> Just (Left x)
               Nothing -> find a r
  where n = W.attrName a
        m = W.attrName (at b)
find a ((b :~ x):r)
  | n == m = case D.fromDynamic (D.toDyn x) of
               Just x  -> Just (Right x)
               Nothing -> find a r
  where n = W.attrName a
        m = W.attrName (at b)
find a (_:r) = find a r

dget :: forall a t m w. (Typeable a, MonadComponent t m) => 
       W.Attr w a -> Component t w -> m (Dynamic t a)
dget a (Component (w,p)) = do
  case find a p of
    Just (Left c)  -> return $ constDyn c
    Just (Right d) -> return d
    Nothing        -> do
                        c <- liftIO $ W.get w a
                        return $ constDyn c

fromwa :: Typeable a => W.Attr w a -> Attr t w a
fromwa a = Attr (dget a) a

towp :: MonadComponent t m => w -> Prop t w -> m (W.Prop w)
towp w (a := v) = return $ (at a) W.:= v
towp w (a :~ v) = do
  addIOEvent $ fmap (\x -> W.set w [(at a) W.:= x]) (updated v)
  cv <- sample $ current v
  return $ (at a) W.:= cv

wrapEvent :: forall t m w. MonadComponent t m => 
             W.Event w (IO ()) -> Component t w -> m (Event t ()) 
wrapEvent e (Component (w,_)) = do
  --TODO Cache events.
  let fire :: EventTrigger t () -> [DSum (EventTrigger t)]
      fire et = [et :=> ()]
  f <- fireEvents fire
  n <- newEventWithTrigger $ \et -> do
         W.set w [W.on e W.:= f et]
         return $ W.set w [W.on e W.:= W.propagateEvent]
  return n

wrapEvent1 :: forall t m w a. MonadComponent t m => 
             W.Event w (a -> IO ()) -> Component t w -> m (Event t a) 
wrapEvent1 e (Component (w,_)) = do
  --TODO Cache events.
  let fire :: (EventTrigger t a,a) -> [DSum (EventTrigger t)]
      fire (et,a) = [et :=> a]
  f <- fireEvents fire
  n <- newEventWithTrigger $ \et -> do
         W.set w [W.on e W.:= \a -> f (et,a)]
         return $ W.set w [W.on e W.:= \_ -> W.propagateEvent]
  return n
