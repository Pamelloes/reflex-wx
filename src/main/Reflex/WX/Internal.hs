{-|
License     : wxWindows Library License
Maintainer  : joshuabrot@gmail.com
Stability   : Experimental
-}
{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, RankNTypes #-}
module Reflex.WX.Internal (host
                          ) where

import Control.Concurrent.MVar

import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.State

import Data.Dependent.Sum
import qualified Data.Dynamic as D
import Data.Typeable

import qualified Graphics.UI.WX as W
import qualified Graphics.UI.WXCore as W hiding (Event)

import Reflex
import Reflex.Host.Class
import Reflex.WX.Class hiding (get)

data AnyComp t = forall w. (W.Widget w) => AC (Component t w)
data ECRec t = forall w a. (Typeable w,Eq w,Typeable (EventMap a)) => 
               ECR (W.Event w a,Component t w) (Event t (EventMap a))

findEvent :: (Typeable w,Eq w,Typeable t,Typeable (EventMap a)) =>
             W.Event w a -> Component t w -> [ECRec t] 
               -> Maybe (Event t (EventMap a))
findEvent e c []     = Nothing
findEvent e c ((ECR (e1,c1) a):rs) 
  | n == m = case D.fromDynamic (D.toDyn (c1,a)) of
               Just (x,y)  -> if c == x then Just y else findEvent e c rs
               Nothing     -> findEvent e c rs
  where n = W.attrName (W.on e)
        m = W.attrName (W.on e1)
findEvent e c (_:rs) = findEvent e c rs

data ComponentState t = ComponentState {
  mvar    :: MVar [DSum (EventTrigger t)],
  parent  :: AnyWindow,
  ioEvent :: [Event t (IO ())],
  lay     :: [W.Layout] -> W.Layout,
  comp    :: [AnyComp t],
  compst  :: [(AnyWindow,[W.Layout]->W.Layout,[AnyComp t])],
  ecache  :: [ECRec t]
}

type ComponentInternal t m = StateT (ComponentState t) m

newtype ComponentM t m a = ComponentM { 
  unCM :: ComponentInternal t m a
} deriving (Functor, Applicative, Monad, MonadIO, MonadFix, Typeable)
instance MonadSample t m => MonadSample t (ComponentM t m) where
  sample = ComponentM . lift . sample
instance MonadHold t m => MonadHold t (ComponentM t m) where
  hold a = ComponentM . lift . (hold a)
instance MonadReflexCreateTrigger t m =>
         MonadReflexCreateTrigger t (ComponentM t m) where
  newEventWithTrigger = ComponentM . lift . newEventWithTrigger
{-
instance MonadReflexHost t m => MonadReflexHost t (ComponentM t m) where
  fireEventsAndRead dm a = ComponentM . lift $ fireEventsAndRead dm a
  subscribeEvent         = ComponentM . lift . subscribeEvent
  runFrame               = ComponentM . lift . runFrame
  runHostFrame           = ComponentM . lift . runHostFrame
-}
instance (Typeable t, Reflex t, MonadIO m, MonadHold t m
         ,MonadReflexCreateTrigger t m, MonadFix m
         ) => MonadComponent t (ComponentM t m) where
  askParent         = do
                         ComponentState{parent=p} <- ComponentM $ get
                         return p
  addIOEvent e      = ComponentM $ modify (\(s@ComponentState{ioEvent=i})->
                                        s{ioEvent=e:i})

  pushComponents n  = ComponentM $ modify f
    where f (s@ComponentState{parent=p,lay=l,comp=c,compst=cs})
            = s{parent=n,lay=W.row 10,comp=[],compst=(p,l,c):cs}
  setLayout l       = ComponentM $ modify (\s->s{lay=l})
  addComponent c    = ComponentM $ modify (\(s@ComponentState{comp=i})->
                                          s{comp=(AC c):i})
  popComponents     = do
                        s@(ComponentState _ _ _ l c (h:r) _) <- ComponentM $ get
                        let (p,m,n)=h
                        let ls = fmap (\(AC (Component (l,_)))->W.widget l) c
                        ComponentM $ put s{parent=p,lay=m,comp=n,compst=r}
                        return $ l (reverse ls)
  cacheEvent e c d  = do
                        s@(ComponentState{ecache=ec}) <- ComponentM $ get
                        case findEvent e c ec of
                          Just ev -> do
                                       liftIO $ print "cache!"
                                       return ev
                          Nothing -> do
                                       a <- d
                                       ComponentM$put s{ecache=(ECR (e,c) a):ec}
                                       return a
  fireEvents f      = do
                        ComponentState{mvar=v} <- ComponentM $ get
                        return $ \a -> putMVar v (f a)

whileM :: Monad m => (m Bool) -> m a -> m ()
whileM c l = do
  v <- c
  if v then l >> whileM c l
  else return ()

host :: ComponentM Spider (HostFrame Spider) a -> IO ()
host c = W.start $ do
  runSpiderHost $ do
    v <- liftIO $ newEmptyMVar
    let istate = ComponentState v undefined [] undefined [] [] []

    (_,s) <- runHostFrame $ runStateT (unCM c) istate
    let ComponentState{ioEvent = ie} = s

    ieh <- subscribeEvent $ mergeWith (>>) ie
    whileM (liftIO W.wxcAppGetTopWindow >>= return . (/=W.objectNull)) $ do
      liftIO $ W.wxcAppYield
      e <- liftIO $ tryTakeMVar v
      case (e) of
        Just e -> do
                    io <- fireEventsAndRead e $ do
                            r <- readEvent ieh
                            case r of
                              Just f  -> f
                              Nothing -> return $ return ()
                    liftIO $ io
        Nothing -> return ()
    liftIO $ W.wxcAppExit
    return ()
