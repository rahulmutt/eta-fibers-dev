{-# LANGUAGE GHCForeignImportPrim, MagicHash, UnboxedTuples, UnliftedFFITypes #-}
module Control.Concurrent.Fiber.Internal where

import GHC.Base
import Control.Monad
import Unsafe.Coerce

import Control.Monad.IO.Class

newtype Fiber a = Fiber { unFiber :: State# RealWorld -> (# State# RealWorld, a #) }

instance Functor Fiber where
  fmap f (Fiber io) = Fiber $ \s -> case io s of (# s1, a #) -> (# s1, f a #)

-- Refine this to fork separate fibers
instance Applicative Fiber where
  pure = return
  (<*>) = ap

instance Monad Fiber where
  (>>=) :: forall a b. Fiber a -> (a -> Fiber b) -> Fiber b
  (>>=) (Fiber m) f = Fiber $ \s ->
    case setCurrentC# (unsafeCoerce m) s of
      s1 -> case pushNextC# (unsafeCoerce f) s1 of
        s2 -> case m s2 of
          (# s3, a #) -> case popNextC# s3 of
            (# s4, f' #) ->
              case setCurrentC# (unsafeCoerce ((unsafeCoerce f' :: (a -> Fiber b)) a)) s4 of
                s5 -> unFiber (f a) s5

instance MonadIO Fiber where
  liftIO :: IO a -> Fiber a
  liftIO (IO m) = Fiber m

runFiber :: Fiber a -> IO a
runFiber (Fiber m) = IO m

foreign import prim "eta.fibers.PrimOps.setCurrentC"
  setCurrentC# :: Any -> State# s -> State# s

foreign import prim "eta.fibers.PrimOps.pushNextC"
  pushNextC# :: Any -> State# s -> State# s

foreign import prim "eta.fibers.PrimOps.popNextC"
  popNextC# :: State# RealWorld -> (# State# RealWorld, Any #)

