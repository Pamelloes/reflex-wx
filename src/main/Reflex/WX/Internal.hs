{-|
Module      : Reflex.WX.Internal
Description : This module contains the code for executing a reflex-wx program.
License     : wxWindows Library License
Maintainer  : joshuabrot@gmail.com
Stability   : Experimental
-}
{-# LANGUAGE ExistentialQuantification, FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes, MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, RankNTypes #-}
module Reflex.WX.Internal (
                          ) where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.State

import qualified Graphics.UI.WX as W

import Reflex
import Reflex.Host.Class
import Reflex.WX.Class

{-
data Prop t w = forall a. W.Attr w a := a
              | forall a. W.Attr w a :~ Dynamic t a
newtype Component t w = Component (W.Layout,[Prop t w])

class ( Reflex t, MonadIO m, MonadSample t m, MonadReflexCreateTrigger t m
      , MonadFix m) => MonadComponent t m | m -> t where
  askParent      :: m (forall a. W.Window a)
  addIOEvent     :: Event t (m ()) -> m ()

  pushComponents :: forall a. W.Window a -> m ()
  setLayout      :: ([W.Layout] -> W.Layout) -> m ()
  addComponent   :: forall w. Component t w -> m ()
  popComponents  :: m (W.Layout)
-}

data AnyComp t = forall w. (W.Widget w) => AC (Component t w)

data ComponentState t m = ComponentState {
  parent  :: AnyWindow,
  ioEvent :: [Event t (m ())],
  lay     :: [W.Layout] -> W.Layout,
  comp    :: [AnyComp t],
  compst  :: [(AnyWindow,[W.Layout]->W.Layout,[AnyComp t])]
}

type ComponentInternal t m1 m = StateT (ComponentState t m1) m

newtype ComponentM t m a = ComponentM { unCM :: ComponentInternal t (ComponentM t m) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix)
instance (Reflex t, MonadSample t m) => MonadSample t (ComponentM t m) where
  sample = ComponentM . lift . sample
instance (Reflex t, MonadHold t m) => MonadHold t (ComponentM t m) where
  hold a = ComponentM . lift . (hold a)
instance (Reflex t, MonadReflexCreateTrigger t m) =>
         MonadReflexCreateTrigger t (ComponentM t m) where
  newEventWithTrigger = ComponentM . lift . newEventWithTrigger
instance (Reflex t, MonadIO m, MonadSample t m, MonadHold t m
         ,MonadReflexCreateTrigger t m, MonadFix m) => MonadComponent t (ComponentM t m) where
  askParent        = do
                        ComponentState{parent=p} <- ComponentM $ get
                        return p
  addIOEvent e     = ComponentM $ modify (\(s@ComponentState{ioEvent=i})->
                                       s{ioEvent=e:i})

  pushComponents n = ComponentM $ modify f
    where f (s@ComponentState{parent=p,lay=l,comp=c,compst=cs})
            = s{parent=n,lay=W.row 10,comp=[],compst=(p,l,c):cs}
  setLayout l      = ComponentM $ modify (\s->s{lay=l})
  addComponent c   = ComponentM $ modify (\(s@ComponentState{comp=i})->
                                         s{comp=(AC c):i})
  popComponents    = do
                        s@(ComponentState _ _ l c (h:cs)) <- ComponentM $ get
                        let (p,m,n)=h
                        let ls = fmap (\(AC (Component (l,_)))->W.widget l) c
                        ComponentM $ put s{parent=p,lay=m,comp=n,compst=cs}
                        return $ l ls
{-
addVoidAction ::  Monad m => Event t (m ()) -> ComponentM t m ()
addVoidAction e = ComponentM $ modify (\(s@ComponentState{voidActions=a})
                                        ->s{voidActions=e:a})

registerProp :: (Reflex t, MonadSample t m, MonadIO m) => [Prop t w] 
                 -> w -> ComponentM t m ()
registerProp p w = do
  props <- sequence $ fmap (towp w) p
  liftIO $ W.set w props
  return ()


wrapEvent :: forall w t m. (MonadReflexCreateTrigger t m,Monad m) => 
             W.Event (W.Object w) (IO ()) -> (W.Object w) -> ComponentM t m (Event t ())
wrapEvent e w = do
  {-let k=hash (e,w)
  h <- ComponentM $ gets (\(a@ComponentState{eventMap=e}) -> M.member k e)
  if h then
    ComponentM $ gets (\(ComponentState{eventMap=e}) -> event (e M.! k) )
  else do-}
    n <- newEventWithTrigger $ \et -> do
           --W.set w [W.on e W.:= fireEvents [ et :=> 
           return $ W.set w [ W.on e W.:= W.propagateEvent ]
    --ComponentM $ modify (\(a@ComponentState{eventMap=e})
    --                      ->a{eventMap=M.insert k (AnyEvent n) e})
    return n

-}
