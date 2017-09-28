module Control.Concurrent.Fiber.Internal where

import GHC.Base
import GHC.Conc.Sync
import Control.Monad
import Unsafe.Coerce

import Data.Typeable
import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class

import Java.Core

-- Fiber

newtype Fiber a = Fiber { unFiber :: State# RealWorld -> (# State# RealWorld, a #) }

instance Functor Fiber where
  fmap f (Fiber io) = Fiber $ \s -> case io s of (# s1, a #) -> (# s1, f a #)

-- Refine this to fork separate fibers
instance Applicative Fiber where
  pure = return
  (<*>) = ap

instance Monad Fiber where
  return :: a -> Fiber a
  return a = Fiber $ \s -> (# s, a #)

  (>>=) :: forall a b. Fiber a -> (a -> Fiber b) -> Fiber b
  (>>=) (Fiber m) f = Fiber $ \s ->
    case setCurrentC# (unsafeCoerce m) s of
      s1 -> case pushNextC# (unsafeCoerce f) s1 of
        s2 -> case m s2 of
          (# s3, a #) -> case popNextC# s3 of
            (# s4, _ #) ->
              case f a of
                fa -> case setCurrentC# (unsafeCoerce fa) s4 of
                  s5 -> unFiber fa s5

instance MonadIO Fiber where
  liftIO :: IO a -> Fiber a
  liftIO (IO m) = Fiber m

-- Fiber Utilities
runFiber :: forall a. Fiber a -> IO (Either (Fiber a) a)
runFiber (Fiber m) = undefined
  -- catch (fmap Right $ IO m) (\(Yield _ fiber) -> return $ Left (unsafeCoerce fiber))

runFiberWithBlock :: forall a. Fiber a -> IO (Either (Bool, Fiber a) a)
runFiberWithBlock (Fiber m) = undefined
--   catch (fmap Right $ IO m) $
--   \(Yield block fiber) -> return $ Left (block, unsafeCoerce fiber)

resumeFiber :: Fiber ()
resumeFiber = Fiber $ \s ->
  case getCurrentC# s of
    (# s1, fiber #) ->
      case (unsafeCoerce fiber) s1 of
        (# s2, a #) -> (# go a s2, () #)
  where go :: Any -> State# s -> State# s
        go a s =
          case popContStack# s of
            (# s1, 1#, cont1 #) ->
              let fa = (unsafeCoerce cont1) a
              in case setCurrentC# fa s1 of
                   s2 -> case fa s2 of
                     (# s3, a' #) -> go a' s3
            (# s1, _, _ #) -> s1

yield :: Fiber a
yield = yield' False

block :: Fiber a
block = yield' True

yield' :: Bool -> Fiber a
yield' block = Fiber $ \s ->
  case popContStack# s of
    (# s1, 1#, current #) ->
      let fa = (unsafeCoerce current) extractYieldError
      in case setCurrentC# (unsafeCoerce fa) s1 of
           s2 -> (# yieldFiber# (dataToTag# block) s2
                 ,  unreachableCodeError #)
    (# s1, _, _ #) -> (# s1, lastYieldError #)
  where extractYieldError =
          error "Attempted to extract a value from a Fiber's yield or block."
        lastYieldError =
          error "You cannot yield or block as the last action of a Fiber."
        unreachableCodeError =
          error "This code should not have been reached."

forkFiber :: Fiber () -> IO ThreadId
forkFiber (Fiber m)= IO $ \s ->
  case fork# m s of (# s1, tid #) -> (# s1, ThreadId tid #)

-- Runtime primitives

data {-# CLASS "java.util.Stack" #-} Stack

type Stack# = Object# Stack

foreign import prim "eta.fibers.PrimOps.getCurrentC"
  getCurrentC# :: State# s -> (# State# s, Any #)

foreign import prim "eta.fibers.PrimOps.setCurrentC"
  setCurrentC# :: Any -> State# s -> State# s

foreign import prim "eta.fibers.PrimOps.pushNextC"
  pushNextC# :: Any -> State# s -> State# s

foreign import prim "eta.fibers.PrimOps.popNextC"
  popNextC# :: State# s -> (# State# s, Any #)

foreign import prim "eta.fibers.PrimOps.popContStack"
  popContStack# :: State# s -> (# State# s, Int#, Any #)

foreign import prim "eta.fibers.PrimOps.yieldFiber"
  yieldFiber# :: Int# -> State# s -> State# s
