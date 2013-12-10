{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad (forever, when, void)
import Control.Monad.Trans (MonadIO(..), lift)
import Control.Concurrent (forkIO, killThread, threadDelay)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)

import qualified Control.Distributed.Process as Cloud
import qualified Control.Distributed.Process.Closure as Cloud
import qualified Control.Distributed.Process.Platform.Time as CT
import qualified Control.Distributed.Process.Platform.Supervisor as SP
import qualified Control.Distributed.Process.Platform.ManagedProcess as MP

import CloudUtil (cloud, cloudAsync, cloudWithRemotableAsync)

--------------------------------------------------------------------------------

toSpec !key !childStart =
  SP.ChildSpec {
      SP.childKey = key
    , SP.childType = SP.Worker
    , SP.childRestart = SP.Transient
    , SP.childStop = SP.TerminateImmediately
    , SP.childStart = childStart
    , SP.childRegName = Nothing
    }

closureChild :: Int -> Cloud.Process ()
closureChild n = do
  liftIO $ print n
  Cloud.expect >>= \(n :: Int) -> return ()

$(Cloud.remotable ['closureChild])

--------------------------------------------------------------------------------

viaClosure :: Int -> IO ()
viaClosure nTimes =
  cloudWithRemotableAsync __remoteTable $ do
    sup <- SP.start (SP.RestartOne SP.defaultLimits) []
    loop sup $ nTimes * 2
  where
    loop _ 0 = return ()
    loop !sup !n
      | even n = do
        childStart <- SP.toChildStart ($(Cloud.mkClosure 'closureChild) n)
        let key = show n
            childSpec = toSpec key childStart
        SP.ChildAdded (SP.ChildRunning _) <- SP.startChild sup childSpec
        loop sup $ n - 1
      | otherwise = do
        let key = show $ n + 1
        SP.terminateChild sup key >>= liftIO . print
        SP.deleteChild sup key >>= liftIO . print
        loop sup $ n - 1

viaSend :: Int ->  IO ()
viaSend nTimes = cloudAsync $ do
    sup <- SP.start (SP.RestartOne SP.defaultLimits) []
    cStart <- SP.toChildStart sendChild
    go sup cStart
  where
    sendChild :: Cloud.Process ()
    sendChild =
        Cloud.expect >>= printInt
      where
        printInt :: Int -> Cloud.Process ()
        printInt !n = liftIO $ print n

    go !sup !childStart = loop sup $ nTimes * 2
      where
        loop _ 0 = return ()
        loop !sup !n
          | even n = do
            let key = show n
                childSpec = toSpec key childStart
            SP.ChildAdded (SP.ChildRunning !cpid) <- SP.startChild sup childSpec
            liftIO $ threadDelay 100
            Cloud.send cpid n
            loop sup $ n - 1
          | otherwise = do
            let key = show $ n + 1
            SP.terminateChild sup key >>= liftIO . print
            SP.deleteChild sup key >>= liftIO . print
            loop sup $ n - 1

--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> hPutStrLn stderr "Number of cases required" >> exitWith (ExitFailure 1)
    (runType : input : _)
      | runType == "closure" -> viaClosure (read input)
      | runType == "send"    -> viaSend (read input)
      | otherwise -> do
          hPutStrLn stderr "Invalid run type (valid options: closure, send)"
          exitWith (ExitFailure 1)
