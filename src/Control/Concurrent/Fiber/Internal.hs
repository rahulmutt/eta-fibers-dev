{-# LANGUAGE GHCForeignImportPrim, MagicHash, UnboxedTuples, UnliftedFFITypes #-}
module Control.Concurrent.Fiber.Internal where

import GHC.Base
import Control.Monad
import Unsafe.Coerce

import Data.Typeable
import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class

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
            (# s4, f' #) ->
              case setCurrentC# (unsafeCoerce ((unsafeCoerce f' :: (a -> Fiber b)) a)) s4 of
                s5 -> unFiber (f a) s5

instance MonadIO Fiber where
  liftIO :: IO a -> Fiber a
  liftIO (IO m) = Fiber m

-- Yield exception
newtype Yield = Yield Any
  deriving Typeable

instance Exception Yield

instance Show Yield where
  show _ = "Yield"

-- Fiber Utilities

runFiber :: forall a. Fiber a -> IO (Either (Fiber a) a)
runFiber (Fiber m) =
  catch (fmap Right $ IO m) (\(Yield fiber) -> return $ Left (unsafeCoerce fiber))

yield :: Fiber a
yield = Fiber $ \s ->
  case getCurrentC# s of
    (# s1, x #) -> case getContStack# s1 of
      (# s2, xs #) ->
        let continuation = (unsafeCoerce (compose (unsafeCoerce xs) (unsafeCoerce x)))
        in unIO (throwIO (Yield continuation)) s2
  where compose :: [a -> Fiber a] -> (a -> Fiber a)
        compose (f:fs) = \x -> (unsafeCoerce f) x >>= compose fs
        compose []     = return

forkFiber :: Fiber () -> IO ThreadId
forkFiber = forkIO . runFiberAndYield

runFiberAndYield :: Fiber () -> IO ()
runFiberAndYield fiber = do
  res <- runFiber fiber
  case res of
    Left continuation -> yieldWith continuation
    Right res         -> return res

yieldWith :: Fiber () -> IO ()
yieldWith fiber = IO $ \s -> (# yieldWith# (unsafeCoerce (runFiberAndYield fiber)) s, () #)

-- Runtime primitives

foreign import prim "eta.fibers.PrimOps.getCurrentC"
  getCurrentC# :: State# s -> (# State# s, Any #)

foreign import prim "eta.fibers.PrimOps.setCurrentC"
  setCurrentC# :: Any -> State# s -> State# s

foreign import prim "eta.fibers.PrimOps.pushNextC"
  pushNextC# :: Any -> State# s -> State# s

foreign import prim "eta.fibers.PrimOps.popNextC"
  popNextC# :: State# s -> (# State# s, Any #)

foreign import prim "eta.fibers.PrimOps.getContStack"
  getContStack# :: State# s -> (# State# s, Any #)

foreign import prim "eta.fibers.PrimOps.yieldWith"
  yieldWith# :: Any -> State# s -> State# s
