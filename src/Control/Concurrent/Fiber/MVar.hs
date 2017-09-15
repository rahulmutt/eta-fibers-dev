{-# LANGUAGE UnboxedTuples, MagicHash #-}
module Control.Concurrent.Fiber.MVar
  (takeMVar, putMVar)
where

import GHC.Base
import Control.Concurrent.Fiber
import GHC.MVar (MVar(..))

takeMVar :: MVar a -> Fiber a
takeMVar (MVar m) = go
  where go = Fiber $ \s ->
               case tryTakeMVar# m s of
                 (# s', 0#, _ #) -> unFiber (yield >> go) s'
                 (# s', _,  a #) -> (# s', a  #)

putMVar :: MVar a -> a -> Fiber ()
putMVar (MVar m) x = go
  where go = Fiber $ \s ->
               case tryPutMVar# m x s of
                 (# s', 0# #) -> unFiber (yield >> go) s'
                 (# s', _  #) -> (# s', () #)
