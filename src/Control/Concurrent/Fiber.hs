module Control.Concurrent.Fiber
  (Fiber(..)
  ,runFiber
  ,forkFiber
  ,yield
  ,block
  ,liftIO

  ,MVar
  ,newMVar
  ,newEmptyMVar
  ,modifyMVar
  ,takeMVar
  ,putMVar

  ,setNumCapabilities
  ,getNumCapabilities

  ,threadDelay
  )
  where

import Control.Concurrent.Fiber.Internal
import Control.Concurrent.Fiber.MVar
import Control.Monad.IO.Class
import GHC.Conc.Sync hiding (yield)
import GHC.Conc.IO
import Control.Concurrent.MVar (newEmptyMVar, newMVar, modifyMVar)
