{-# LANGUAGE ScopedTypeVariables #-}
module CloudUtil
       ( module Control.Distributed.Process
       , cloud
       , cloudAsync
       , cloudWithRemotable
       , cloudWithRemotableAsync)
       where

import Control.Exception (try, SomeException, bracket)
import Control.Concurrent (threadDelay)
import Control.Monad (void, forM_)
import qualified Control.Concurrent.MVar as MVar
import Control.Distributed.Process hiding (try, bracket)
import qualified Control.Distributed.Process.Node as Cloud

import qualified Network.Transport      as Trans (Transport, closeTransport)
import qualified Network.Transport.Chan as Trans
-- import qualified Network.Transport.TCP as Trans

mkLocalNode :: (RemoteTable -> RemoteTable) -> IO (Cloud.LocalNode, Trans.Transport)
mkLocalNode remoteTable = do
  trans <- Trans.createTransport
  -- Right trans <- Trans.createTransport "localhost" "557766" Trans.defaultTCPParameters
  localNode <- Cloud.newLocalNode trans $ remoteTable Cloud.initRemoteTable
  return (localNode, trans)

cloudWithRemotable
  :: (RemoteTable -> RemoteTable) -> Process b -> IO b
cloudWithRemotable remoteTable cproc = do
  (node, trans) <- mkLocalNode remoteTable
  resultVar <- MVar.newEmptyMVar
  Cloud.runProcess node (cproc >>= liftIO . MVar.putMVar resultVar)
  threadDelay 100000
  result <- MVar.takeMVar resultVar
  (_ :: Either SomeException ()) <- try $ Cloud.closeLocalNode node
  (_ :: Either SomeException ()) <- try $ Trans.closeTransport trans
  return result

cloudWithRemotableAsync :: (RemoteTable -> RemoteTable) -> Process () -> IO ()
cloudWithRemotableAsync remoteTable cproc =
  bracket (mkLocalNode remoteTable)
          (\(node, trans) -> do
              (_ :: Either SomeException ()) <- try $ Cloud.closeLocalNode node
              (_ :: Either SomeException ()) <- try $ Trans.closeTransport trans
              return ())
          (\(node, _) -> Cloud.runProcess node $ do
              cproc
              liftIO $ putStrLn "Start delay to clear up space"
              forM_ [1..2000] $ (\_ -> liftIO $ threadDelay 1000))

cloud :: Process b -> IO b
cloud = cloudWithRemotable id

cloudAsync :: Process () -> IO ()
cloudAsync = cloudWithRemotableAsync id
