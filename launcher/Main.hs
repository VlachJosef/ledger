{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative
--import Control.Concurrent
import GHC.IO.Exception
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Conversion as DBC
import System.Process.Typed
import Data.Semigroup ((<>))
import Control.Monad.State.Strict
import Serokell.Communication.IPC (NodeId(..))
import System.IO
import Data.List
import System.Console.Haskeline
import System.Console.Haskeline.History
import System.Directory
import Data.Either.Validation
import Control.Exception
import System.FilePath ((</>))
import qualified Text.Parsec as Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator

binDir     :: FilePath
binDir      = "./.stack-work/install/x86_64-osx/lts-10.3/8.2.2/bin/"
genExec    :: FilePath
genExec     = binDir <> "crypto-ledger-keys-generator"
ncExec     :: FilePath
ncExec      = binDir <> "crypto-nc"
clientExec :: FilePath
clientExec  = binDir <> "crypto-ledger-client"
nodeExec   :: FilePath
nodeExec    = binDir <> "crypto-ledger-node"

pattern Script    :: String
pattern Script    = "script"
pattern Cluster   :: String
pattern Cluster   = "cluster"
pattern Launch    :: String
pattern Launch    = "launch"
pattern Stop      :: String
pattern Stop      = "stop"
pattern Terminate :: String
pattern Terminate = "terminate"
pattern Status    :: String
pattern Status    = "status"
pattern All       :: String
pattern All       = "all"

data CommandInfo
  = CommandInfo { command :: String
                , takeParams :: Bool
                }

data ClusterCmd
  = ClusterLaunch Int
  | ClusterNodeLaunch
  | ClusterNodeStop NodeId
  | ClusterTerminate
  | ClusterStatus
  | ClusterScript
  deriving (Show)

data ClientCmd
  = Submit String Int
  | ClientStatus
  | QueryTx String
  | Balance String
  deriving (Show)

data QueryNode
  = QueryAll ClientCmd
  | QueryOne NodeId ClientCmd
  deriving (Show)

data Cmd
  = ClusterCmd ClusterCmd
  | QueryNodeCmd QueryNode
  | InvalidCmd String Parsec.ParseError
  | EmptyCmd
  deriving (Show)


pDigits :: Parsec.Parsec String () String
pDigits = many1 digit

pIntPositive :: Parsec.Parsec String () Int
pIntPositive = do
  digits <- pDigits
  case validNumber digits of
    Failure errors -> Parsec.parserFail (unwords errors)
    Success n -> pure n

pNodeId :: Parsec.Parsec String () NodeId
pNodeId = NodeId <$> pIntPositive

pStringWithParam :: String -> Parsec.Parsec String () String
pStringWithParam str = (string str *> Parsec.spaces *> many1 alphaNum)

pClientSubmit :: Parsec.Parsec String () ClientCmd
pClientSubmit = Submit <$> pStringWithParam "submit" <*> (Parsec.spaces *> pIntPositive)

pClientStatus :: Parsec.Parsec String () ClientCmd
pClientStatus = ClientStatus <$ string "status"

pClientQueryTx :: Parsec.Parsec String () ClientCmd
pClientQueryTx = QueryTx <$> pStringWithParam "query"

pClientBalance :: Parsec.Parsec String () ClientCmd
pClientBalance = Balance <$> pStringWithParam "balance"

pClientCmd :: Parsec.Parsec String () ClientCmd
pClientCmd
  =   Parsec.try pClientStatus -- 'status' clash with 'submit', we don't want consume anything
  <|> pClientQueryTx
  <|> pClientSubmit
  <|> pClientBalance

pQueryNodeAll :: Parsec.Parsec String () QueryNode
pQueryNodeAll = QueryAll <$> (string "all" *> Parsec.spaces *> pClientCmd)

pQueryNodeJustNode :: Parsec.Parsec String () QueryNode
pQueryNodeJustNode = QueryOne <$> pNodeId <*> (Parsec.spaces *> pClientCmd)

pQueryNode :: Parsec.Parsec String () QueryNode
pQueryNode
  =   pQueryNodeAll
  <|> pQueryNodeJustNode

pClusterLaunch :: Parsec.Parsec String () ClusterCmd
pClusterLaunch = ClusterLaunch <$> (string Cluster *> Parsec.spaces *> pIntPositive)

pClusterStatus :: Parsec.Parsec String () ClusterCmd
pClusterStatus = ClusterStatus <$ string "status"

pClusterScript :: Parsec.Parsec String () ClusterCmd
pClusterScript = ClusterScript <$ string "script"

pClusterLaunchNode :: Parsec.Parsec String () ClusterCmd
pClusterLaunchNode = ClusterNodeLaunch <$ string Launch

pClusterNodeStop :: Parsec.Parsec String () ClusterCmd
pClusterNodeStop = ClusterNodeStop <$> (string Stop *> Parsec.spaces *> pNodeId)

pClusterTerminate :: Parsec.Parsec String () ClusterCmd
pClusterTerminate = ClusterTerminate <$ string Terminate

pClusterCmd :: Parsec.Parsec String () ClusterCmd
pClusterCmd
  =   Parsec.try pClusterStatus
  <|> pClusterScript
  <|> pClusterLaunch
  <|> pClusterLaunchNode
  <|> pClusterNodeStop
  <|> pClusterTerminate

pCmd :: Parsec.Parsec String () Cmd
pCmd
  =   ClusterCmd   <$> pClusterCmd
  <|> QueryNodeCmd <$> pQueryNode

sourceName :: String
sourceName = "launcher-params"

ciScript    :: CommandInfo
ciScript     = CommandInfo Script    False
ciCluster   :: CommandInfo
ciCluster    = CommandInfo Cluster   True
ciLaunch    :: CommandInfo
ciLaunch     = CommandInfo Launch    False
ciStop      :: CommandInfo
ciStop       = CommandInfo Stop      True
ciTerminate :: CommandInfo
ciTerminate  = CommandInfo Terminate True
ciStatus    :: CommandInfo
ciStatus     = CommandInfo Status    False
ciAll       :: CommandInfo
ciAll        = CommandInfo All       True
ciSubmit    :: CommandInfo
ciSubmit     = CommandInfo "submit"  True
ciBalance   :: CommandInfo
ciBalance    = CommandInfo "balance" True
ciQuery     :: CommandInfo
ciQuery      = CommandInfo "query"   True

commands :: [CommandInfo]
commands = [ciScript, ciCluster, ciLaunch, ciStop, ciTerminate, ciStatus, ciAll]

commandsAfterAll :: [CommandInfo]
commandsAfterAll = [ciStatus, ciSubmit, ciBalance, ciQuery]

settings :: Settings (StateT RunningEnvironment IO)
settings = setComplete comp defaultSettings

comp :: (String, String) -> StateT RunningEnvironment IO ([Char], [Completion])
comp (onLeft, onRight) = do
  s <- get
  let
    rOnLeft :: String
    rOnLeft = reverse onLeft

    clientNodeIds :: [String]
    clientNodeIds = (show . nodeId . processData) <$> runningClientProcesses s

    firstOrderCommands :: [CommandInfo]
    firstOrderCommands = commands ++ ((\a -> (CommandInfo a True)) <$> clientNodeIds)

    clientCommand :: Maybe String
    clientCommand = find (\a -> take (length a) rOnLeft == a) ((\a -> a <> " ") <$> ("all" : clientNodeIds))

    (commandInfos, prefix) = case clientCommand of
      Nothing    -> (filter (\a -> isPrefixOf rOnLeft (command a)) firstOrderCommands, "")
      Just match -> (filter (\a -> isPrefixOf (drop (length match) rOnLeft) (command a)) commandsAfterAll, match)

    in pure (reverse prefix, (\CommandInfo{..} -> Completion command command takeParams) <$> commandInfos)

main :: IO ()
main = evalStateT (runInputT settings loop) (EnvData [] [] Nothing [])
   where
       loop :: InputT (StateT RunningEnvironment IO) ()
       loop = do
           s <- lift $ get
           let (scriptCommand, rest) = maybe ("", []) id (uncons (script s))
           lift $ put $ s { script = rest
                          }
           let sc = if null scriptCommand
                    then getInputLine ">>> "
                    else do
                          modifyHistory (addHistory scriptCommand)
                          pure $ Just scriptCommand

           minput <- sc
           case minput of
               Nothing     -> pure ()
               Just input  -> do
                 (lift . execCommand . parseCmd) input
                 loop

data ProcessData
  = ProcessData { nodeId :: NodeId
                , processConfig :: ProcessConfig () () ()
                } deriving (Show)

data RunningProcess
  = RunningProcess { processData :: ProcessData
                   , runningProcess :: Process () () ()
                   }

instance Show RunningProcess where
  show RunningProcess{..} = "Running process nodeId: " <> (show . nodeId) processData <> ", config " <> (show . processConfig) processData

instance Show NodeId where
  show (NodeId nId) = show nId

data RunningEnvironment =
  EnvData { runningProcesses :: [RunningProcess]
          , runningClientProcesses :: [RunningProcess]
          , stoppedNode :: Maybe ProcessData
          , script :: [String]
          }

parseCmd :: String -> Cmd
parseCmd s = if null s
               then EmptyCmd
               else case Parsec.parse pCmd sourceName s of
                      Left errors -> InvalidCmd s errors
                      Right cmd -> cmd

validNumber :: String -> Validation [String] Int
validNumber s = case DBC.fromByteString (BSC.pack s) of
                  Nothing -> Failure ["Expected number, got: '" <> s <> "'"]
                  Just n  -> Success n

runNode :: ProcessData -> IO RunningProcess
runNode pd = do
  process <- (startProcess . processConfig) pd
  pure $ RunningProcess pd process

runNodes :: [ProcessData] -> IO [RunningProcess]
runNodes xs = sequence $ runNode <$> xs

prepareNodes :: Int -> [ProcessData]
prepareNodes n = [ ProcessData (NodeId i) (proc nodeExec [show i, show n, "sockets", "0", "0", "0", "Distribution.keys"])
     | i <- [0 .. (n - 1)]
     ]

prepareNodesClient :: Int -> [FilePath] -> [ProcessData]
prepareNodesClient n fps = [ ProcessData (NodeId clientNodeId) (proc clientExec ["-c", show clientNodeId, "-n", show i, "-k", privateKeyPath])
     | (i, privateKeyPath) <- (zip [0 .. (n - 1)] fps),
       let clientNodeId = i + 100
     ]

keyGenerator :: Int -> ProcessConfig () () ()
keyGenerator n = proc genExec [show n]

terminateProcesses :: [RunningProcess] -> IO [ExitCode]
terminateProcesses xs = do
  putStrLn $ "Terminating " <> show (length xs) <> " processes."
  sequence $ terminateProcess . runningProcess <$>  xs

terminateProcess :: Process () () () -> IO ExitCode
terminateProcess ps = do
  putStrLn $ "Terminating process: " <> show ps
  stopProcess ps
  waitExitCode ps

relaunchProcess :: StateT RunningEnvironment IO ()
relaunchProcess = do
  s <- get
  case stoppedNode s of
    Just processData -> do
      runningProcess <- lift $ (runNode processData)
      put $ s { runningProcesses = runningProcess : (runningProcesses s)
              , stoppedNode = Nothing
              }
      lift $ putStrLn $ "Process nodeId " <> show (nodeId processData) <> " relaunched."
    Nothing  -> lift $ putStrLn $ "No process has been stopped."

killProcess :: NodeId -> StateT RunningEnvironment IO ()
killProcess nodeIdToStop = do
  s <- get
  let nodeIdToStopPred runningProcess = (nodeId . processData) runningProcess == nodeIdToStop
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

checkPrivateKeys :: Int -> IO ()
checkPrivateKeys n = do
  cd       <- getCurrentDirectory
  allFiles <- listDirectory (cd </> keysDir)
  let keys = filter (\a -> length a == 128) allFiles
  if length keys == n
    then pure ()
    else void $ runProcess $ keyGenerator n

launchCluster :: Int -> StateT RunningEnvironment IO ()
launchCluster n = do
    s <- get
    lift $ checkPrivateKeys n
    if null (runningProcesses s) then do
      processes <- lift $ runNodes (prepareNodes n)
      lift $ void $ sequence $ putStr . show  <$> processes
      put $ s { runningProcesses = processes }
    else lift $ putStrLn $ "Cluster already launched! " <> show (runningProcesses s)

keysDir :: String
keysDir = "keys"

launchClient :: StateT RunningEnvironment IO ()
launchClient = do
  --lift $ threadDelay $ 1 * 1000 * 1000
  s <- get
  if null (runningClientProcesses s) then do
    cd <- lift $ getCurrentDirectory
    allFiles <- lift $ listDirectory (cd </> keysDir)
    let keys      = filter (\a -> length a == 128) allFiles
    let processes = runningProcesses s
    let psData    = prepareNodesClient (length processes) ((\e -> keysDir </> e) <$> keys)
    runningProcesses <- lift $ runNodes psData
    lift $ void $ sequence $ putStr . show  <$> runningProcesses
    put $ s { runningClientProcesses = runningProcesses }
  else lift $ putStrLn $ "Client already launched! " <> show (runningClientProcesses s)

terminateCluster :: StateT RunningEnvironment IO ()
terminateCluster = do
    s <- get
    exitCodes <- lift $ terminateProcesses (runningClientProcesses s ++ runningProcesses s)
    lift $ putStrLn $ "Exit codes: " <> show (show <$> exitCodes)
    put (EnvData [] [] Nothing [])

status :: StateT RunningEnvironment IO ()
status = do
    s <- get
    let procs = runningProcesses s
    let clientProcs = runningClientProcesses s
    lift $ putStrLn $ "Node processes: "
    lift $ void     $ sequence $ putStr . show <$> procs
    lift $ putStrLn $ "\nClient processes: "
    lift $ void     $ sequence $ putStr . show <$> clientProcs
    lift $ putStrLn $ "\nStopped node: " <> show (stoppedNode s)

runScript :: StateT RunningEnvironment IO ()
runScript = do
  s <- get
  put $ s { script = scriptData
          }

execClusterCmd :: ClusterCmd -> StateT RunningEnvironment IO ()
execClusterCmd  = \case
  ClusterLaunch n     -> launchCluster n *> launchClient
  ClusterNodeLaunch   -> relaunchProcess
  ClusterNodeStop nId -> killProcess nId
  ClusterTerminate    -> terminateCluster
  ClusterStatus       -> status
  ClusterScript       -> runScript

execQueryClientCmd :: QueryNode -> StateT RunningEnvironment IO ()
execQueryClientCmd = \case
  QueryAll clientCmd -> do
    s <- get
    void . sequence $ (\nId -> execClientCmd nId clientCmd) <$> nodeId . processData <$> runningClientProcesses s
  QueryOne nodeId clientCmd -> execClientCmd nodeId clientCmd

execClientCmd :: NodeId -> ClientCmd -> StateT RunningEnvironment IO ()
execClientCmd clientId = \case
  Submit address amount -> lift $ runClientCmd clientId ("SUBMIT " <> address <> " " <> show amount)
  ClientStatus          -> lift $ runClientCmd clientId "status"
  QueryTx txId          -> lift $ runClientCmd clientId ("QUERY " <> txId)
  Balance address       -> lift $ runClientCmd clientId ("BALANCE " <> address)

execCommand :: Cmd -> StateT RunningEnvironment IO ()
execCommand = \case
  EmptyCmd                  -> lift (pure ())
  InvalidCmd cmd errors     -> lift $ putStrLn $ "InvalidCmd: " <> show cmd <> ", parser error: " <> show errors
  QueryNodeCmd queryNodeCmd -> execQueryClientCmd queryNodeCmd
  ClusterCmd clusterCmd     -> execClusterCmd clusterCmd

runClientCmd :: NodeId -> String -> IO ()
runClientCmd clientId s = let
  nc :: ProcessConfig () Handle ()
  nc = setStdin (byteStringInput (BSL.fromStrict $ BSC.pack s)) $ setStdout createPipe $ proc ncExec [(show . unNodeId) clientId]
  in do

  withProcess_ nc $ \p -> do
      out <- hGetContents $ getStdout p
      void . evaluate $ length out -- lazy I/O :(
      mapM_ putStrLn $ take 200 $ lines out

scriptData :: [String]
scriptData = [ "cluster 3"
             , "all status"
             , "all submit 97af2031b33efb033c7c377c53dc94a22c6be60839efb23201a9fdda2b8ab785 500"
             ]
