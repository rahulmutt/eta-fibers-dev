module Control.Concurrent.Fiber.Internal where

import GHC.Base
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

-- Yield exception
-- True - Block
-- False - Yield
data Yield = Yield Bool Any
  deriving Typeable

instance Exception Yield

instance Show Yield where
  show _ = "Yield"

-- Fiber Utilities

runFiber :: forall a. Fiber a -> IO (Either (Fiber a) a)
runFiber (Fiber m) =
  catch (fmap Right $ IO m) (\(Yield _ fiber) -> return $ Left (unsafeCoerce fiber))

runFiberWithBlock :: forall a. Fiber a -> IO (Either (Bool, Fiber a) a)
runFiberWithBlock (Fiber m) =
  catch (fmap Right $ IO m) $
  \(Yield block fiber) -> return $ Left (block, unsafeCoerce fiber)

yield :: Fiber a
yield = yield' False

block :: Fiber a
block = yield' True

yield' :: Bool -> Fiber a
yield' block = Fiber $ \s ->
  case getContStack# s of
    (# s1, stack# #) ->
      case mkFiber# stack# s1 of
        (# s2, continuation #) ->
          unIO (throwIOWithoutStack (Yield block (unsafeCoerce continuation))) s2

mkFiber# :: Stack# -> State# s -> (# State# s, Fiber a #)
mkFiber# stack# s =
  case popContStack# stack# s of
    (# s1, 1#, cont1 #) ->
      go ((unsafeCoerce cont1)
          (error "Attempted to extract a value from a Fiber's yield or block.")) s1
    (# s1, _,  _ #) ->
      (# s1, error "You cannot yield or block as the last action of a Fiber." #)
  where go :: Fiber a -> State# s -> (# State# s, Fiber a #)
        go fiber s =
          case popContStack# stack# s of
            (# s1, 1#, cont #) -> go (fiber >>= (unsafeCoerce cont)) s1
            (# s1, _, _     #) -> (# s1, fiber #)

forkFiber :: Fiber () -> IO ThreadId
forkFiber = forkIO . runFiberAndYield

runFiberAndYield :: Fiber () -> IO ()
runFiberAndYield fiber = do
  res <- runFiberWithBlock fiber
  case res of
    Left (block, continuation) -> yieldWith block continuation
    Right res         -> return res

yieldWith :: Bool -> Fiber () -> IO ()
yieldWith block fiber = IO $ \s ->
  (# yieldWith# (unsafeCoerce (runFiberAndYield fiber)) block# s, () #)
  where block# = if block then 1# else 0#

throwIOWithoutStack :: Exception e => e -> IO a
throwIOWithoutStack e = IO $ \s ->
  case raiseIOWithoutStackTrace# (unsafeCoerce (toException e)) s of
    (# s1, a #) -> (# s1, unsafeCoerce a #)

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

foreign import prim "eta.fibers.PrimOps.getContStack"
  getContStack# :: State# s -> (# State# s, Stack# #)

foreign import prim "eta.fibers.PrimOps.popContStack"
  popContStack# :: Stack# -> State# s -> (# State# s, Int#, Any #)

foreign import prim "eta.fibers.PrimOps.yieldWith"
  yieldWith# :: Any -> Int# -> State# s -> State# s

foreign import prim "eta.fibers.PrimOps.raise"
  raiseIOWithoutStackTrace# :: Any -> State# s -> (# State# s, Any #)
