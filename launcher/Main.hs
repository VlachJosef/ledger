{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

module Main where

import Control.Concurrent
import Control.Exception (bracket, try)
import GHC.IO.Exception
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Conversion as DBC
import System.Process.Typed
import Data.Semigroup
import Control.Monad.State
import Serokell.Communication.IPC
import System.IO
import Data.List

main :: IO ()
main = do
  (a, b) <- runStateT loop (XXX [] Nothing)
  pure ()

data ProcessData
  = ProcessData { nodeId :: NodeId
                , processConfig :: ProcessConfig () () ()
                } deriving (Show)

data ProcessInfo
  = ProcessInfo { processData :: ProcessData
                , runningProcess :: Process () () ()
                } deriving (Show)

instance Show NodeId where
    show (NodeId nId) = show nId

data RunningEnvironment =
  XXX { runningProcesses :: [ProcessInfo]
      , stoppedNode :: Maybe ProcessData
  }

data Cmd
  = LaunchCluster Int
  | LaunchNode
  | StopNode NodeId
  | TerminateCluster
  | Status
  | InvalidCmd String
  | EmptyCmd

parseNumber :: String -> (NodeId -> Cmd) -> Cmd
parseNumber maybeNumber f = case DBC.fromByteString (BSC.pack maybeNumber) of
                              Nothing -> InvalidCmd ("NodeId must be a number, got: " <> maybeNumber)
                              Just n -> f (NodeId n)

parseCmd :: String -> Cmd
parseCmd input = case words input of
  ["cluster", amount] -> case DBC.fromByteString (BSC.pack amount) of
                           Nothing -> InvalidCmd ("Amount must be a number, got: " <> amount)
                           Just n
                             | n <= 100  -> LaunchCluster n
                             | otherwise -> InvalidCmd ("Cluster cannot be bigger than 100 nodes, got: " <> amount)
  ["cluster"]   -> LaunchCluster 5
  ["c"]         -> LaunchCluster 5
  ["launch"]    -> LaunchNode
  ["stop",   nodeIdAsStr] -> parseNumber nodeIdAsStr StopNode
  ["terminate"] -> TerminateCluster
  ["t"]         -> TerminateCluster
  ["status"]    -> Status
  ["s"]         -> Status
  []            -> EmptyCmd
  other         -> InvalidCmd (unwords other)

runNode :: ProcessData -> IO ProcessInfo
runNode pd = do
  process <- (startProcess . processConfig) pd
  pure $ ProcessInfo pd process

runNodes :: [ProcessData] -> IO [ProcessInfo]
runNodes xs = sequence $ runNode <$> xs

prepareNodes :: Int -> [ProcessData]
prepareNodes n = let
  nodeExec = "./.stack-work/install/x86_64-osx/lts-10.3/8.2.2/bin/crypto-ledger-node"
  in [ ProcessData (NodeId i) (proc nodeExec [show i, show n, "sockets", "0", "0", "0", "Distribution.keys"]) | i <- [0 .. (n - 1)]]

terminateProcesses :: [ProcessInfo] -> IO [ExitCode]
terminateProcesses xs = do
  putStrLn $ "Terminating " <> show (length xs) <> " processes."
  sequence $ terminateProcess . runningProcess <$>  xs

terminateProcess :: Process () () () -> IO ExitCode
terminateProcess ps = do
  putStrLn $ "Terminating process: " <> show ps
  stopProcess ps
  waitExitCode ps

loop :: StateT RunningEnvironment IO ()
loop = do
  lift $ putStr "Enter command: "
  lift $ hFlush stdout
  cmd <- nextCommand
  execCommand cmd
  loop

relaunchProcess :: StateT RunningEnvironment IO ()
relaunchProcess = do
  s <- get
  case stoppedNode s of
    Just processData -> do
      processInfo <- lift $ (runNode processData)
      put $ s { runningProcesses = processInfo : (runningProcesses s)
              , stoppedNode = Nothing
              }
      lift $ putStrLn $ "Process nodeId " <> show (nodeId processData) <> " relaunched."
    Nothing  -> lift $ putStrLn $ "No process has been stopped."

killProcess :: NodeId -> StateT RunningEnvironment IO ()
killProcess nodeIdToStop = do
  s <- get
  let nodeIdToStopPred pi = (nodeId . processData) pi == nodeIdToStop
  case stoppedNode s of
    Just nId -> case find nodeIdToStopPred (runningProcesses s) of
      Just _  -> lift $ putStrLn $ "You can stop only one process at a time. Process " <> show nId <> " is already stopped."
      Nothing -> lift $ putStrLn $ "Process " <> show nId <> " is already stopped."
    Nothing -> do
      let (toKill, toKeep) = partition nodeIdToStopPred (runningProcesses s)
      case toKill of
        [toK] -> do
          lift $ void $ (terminateProcess . runningProcess) toK
          put $ s { runningProcesses = toKeep
                  , stoppedNode = Just (processData toK)
                  }
        [] -> lift $ putStrLn $ "No running process with nodeId " <> show nodeIdToStop <> " found."
        _  -> lift $ putStrLn $ "Error. More than one process with nodeId " <> show nodeIdToStop <> " found."

launchCluster :: Int -> StateT RunningEnvironment IO ()
launchCluster n = do
    s <- get
    if null (runningProcesses s) then do
      processes <- lift $ runNodes (prepareNodes n)
      lift $ void $ sequence $ putStr . show  <$> processes
      put $ s { runningProcesses = processes }
    else lift $ putStrLn $ "Cluster already launched!" <> show (runningProcesses s)

terminateCluster :: StateT RunningEnvironment IO ()
terminateCluster = do
    s <- get
    exitCodes <- lift $ terminateProcesses (runningProcesses s)
    lift $ putStrLn $ "EXIT CODES: " <> show (show <$> exitCodes)
    put (XXX [] Nothing)

status :: StateT RunningEnvironment IO ()
status =  do
    s <- get
    let procs = runningProcesses s
    if null procs
      then lift $ putStrLn "No running process."
      else do
       lift $ void $ sequence $ putStr . show <$> procs
       lift $ putStrLn $ "Stopped node: " <> show (stoppedNode s)

execCommand :: Cmd -> StateT RunningEnvironment IO ()
execCommand = \case
  LaunchCluster n  -> launchCluster n
  TerminateCluster -> terminateCluster
  Status           -> status
  LaunchNode       -> relaunchProcess
  StopNode nId     -> killProcess nId
  EmptyCmd         -> lift (pure ())
  InvalidCmd cmd   -> lift $ putStrLn $ "InvalidCmd: " <> show cmd

nextCommand :: StateT RunningEnvironment IO Cmd
nextCommand = parseCmd <$> lift getLine
