{-# LANGUAGE RecursiveDo #-}
module Control.Monad.Indexed.Tardis where

import Control.Monad.Fix
import Control.Monad.Indexed
import Control.Monad.Indexed.Fix
import Control.Monad.Indexed.Trans
import Data.Functor.Identity

newtype ITardisT bw fw m i j a = ITardisT {runITardisT :: (bw j, fw i) -> m (a, (bw i, fw j))}

type ITardis bw fw = ITardisT bw fw Identity

instance Monad m => IxFunctor (ITardisT bw fw m) where
  imap f m = ITardisT $ \ ~(bw, fw) -> do
    (x, ~(bw', fw')) <- runITardisT m (bw, fw)
    return (f x, (bw', fw'))

instance Monad m => IxPointed (ITardisT bw fw m) where
  ireturn x = ITardisT $ \s -> return (x, s)

instance MonadFix m => IxApplicative (ITardisT bw fw m) where
  iap mf mx = ITardisT $ \ ~(bw, fw) -> do
    rec (f, ~(bw'', fw' )) <- runITardisT mf (bw', fw )
        (x, ~(bw' , fw'')) <- runITardisT mx (bw , fw')
    return (f x, (bw'', fw''))

instance MonadFix m => IxMonad (ITardisT bw fw m) where
  ibind f m = ITardisT $ \ ~(bw, fw) -> do
    rec (x,  ~(bw'', fw' )) <- runITardisT m (bw', fw)
        (x', ~(bw',  fw'')) <- runITardisT (f x) (bw, fw')
    return (x', (bw'', fw''))

instance MonadFix m => IxMonadFix (ITardisT bw fw m) where
  imfix f = ITardisT $ \s -> do
    rec  (x, s') <- runITardisT (f x) s
    return (x, s')

instance IxMonadTrans (ITardisT bw fw) where
  ilift m = ITardisT $ \s -> do
    x <- m
    return (x, s)
