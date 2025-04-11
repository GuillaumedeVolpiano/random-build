{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module GHRB.IO
  ( randomBuild
  , terminate
  , allPackages
  , currentUntried
  ) where

import qualified Data.ByteString.Char8         as BS (pack)
import           Data.HashMap.Strict           (keysSet)
import qualified Data.HashSet                  as Set (difference, intersection,
                                                       map, size)
import           Data.List                     (uncons, delete)
import qualified Data.Text                     as T (unpack)
import qualified Data.Text.Lazy                as TL (pack)
import           Data.Time.Clock               (UTCTime)
import           Distribution.Portage.Types    (Package (Package, getVersion),
                                                Repository (Repository))
import           Effectful                     (Eff, (:>))
import           Effectful.Concurrent          (Concurrent)
import           Effectful.FileSystem          (FileSystem)
import           Effectful.Process             (Process,
                                                readProcessWithExitCode)
import           Effectful.Reader.Static       (Reader, asks)
import           Effectful.State.Static.Shared (State, get, gets, modify)
import           Effectful.Time                (Time, currentTime)
import           FlatParse.Basic               (Result (OK))
import           GHRB.Core                     (addTried, failedResolve,
                                                filePathPackage, hasCompleted,
                                                hasDowngraded, hasFailed,
                                                parseDowngrades,
                                                parsePackageList, prettyPackage,
                                                toDate, updateInstalled)
import           GHRB.Core.Types               (
                                                EmergeResult (BuildFailed, EmergeSuccess),
                                                PackageSet,
                                                PrelimEmergeResult (PrelimEmergeSuccess, ResolveFailed, TriedToDowngrade),
                                                Running (Running, Terminated),
                                                St, Stderr, Stdout, failPackage,
                                                getAllPackages, getEmerge,
                                                getHU, getPquery, installed,
                                                package, prettyPrintSt,
                                                successPackage, untried, Args)
import           GHRB.Core.Utils               (prettyMessage, prettyPreMerge,
                                                prettyTry)
import           GHRB.IO.Cmd                   (defaultEmergeArgs,
                                                defaultHUArgs,
                                                defaultPqueryArgs,
                                                installedArgs, repo,
                                                runTransparent)
import           GHRB.IO.Utils                 (bStderr, bStdout, logOutput,
                                                stderr, stdout)
import           RevdepScanner                 (ConstraintMap,
                                                MatchMode (Matching, NonMatching),
                                                argsR, lookupResults, parseAll)
import           System.Exit                   (ExitCode (ExitFailure, ExitSuccess))

import Debug.Trace

tmpLogRoot :: String
tmpLogRoot = "/tmp/random-pkg-"

runPquery ::
     (Process :> es, Reader Args :> es)
  => [String]
  -> Eff es (ExitCode, Maybe PackageSet, String)
runPquery args = do
  pquery <- asks getPquery
  let args' = args ++ defaultPqueryArgs
  (exitCode, packageList, stdErr) <- readProcessWithExitCode pquery args' ""
  case exitCode of
    ExitSuccess -> pure (exitCode, parsePackageList packageList, stdErr)
    _           -> pure (exitCode, Nothing, stdErr)

allPackages :: (Process :> es) => Maybe Package -> FilePath -> Eff es (Maybe PackageSet)
allPackages pkg pquery = case pkg of
                           Nothing -> do
                                      let args = defaultPqueryArgs ++ ["--repo", repo]
                                      (exitCode, packageList, _) <- readProcessWithExitCode pquery args ""
                                      case exitCode of
                                        ExitSuccess -> pure (parsePackageList packageList)
                                        _           -> pure Nothing
                           Just p -> Just <$> revdeps pquery p

runEmerge ::
     (Process :> es, Reader Args :> es)
  => [String]
  -> Package
  -> Eff es (ExitCode, String, String)
runEmerge args pkg =
  asks getEmerge >>= \emerge ->
    readProcessWithExitCode
      emerge
      (defaultEmergeArgs ++ args ++ [T.unpack . prettyPackage $ pkg])
      ""

runHaskellUpdater ::
     (FileSystem :> es, Process :> es, Reader Args :> es, Concurrent :> es)
  => Eff es (ExitCode, Stdout, Stderr)
runHaskellUpdater =
  asks getHU >>= \haskellUpdater -> runTransparent haskellUpdater defaultHUArgs

currentUntried ::
     (FileSystem :> es, Reader Args :> es, Process :> es)
  => Eff es (Either Running PackageSet)
currentUntried = do
  ap <- asks getAllPackages
  rawInstalled <- currentInstalled
  case rawInstalled of
    Left _     -> error "failed to parse list of installed packages"
    Right inst -> pure . Right $ Set.difference ap inst

revdeps :: Process :> es => FilePath -> Package -> Eff es PackageSet
revdeps pquery p = do
  let args' = delete "--unfiltered" . delete "--raw" . argsR $ Repository repo
  (_, out, _) <- traceShow args' readProcessWithExitCode pquery args' ""
  let m =
        case parseAll . TL.pack $ out of
          Right (rm :: ConstraintMap) -> rm
          Left _                      -> error "failed to parse contraint map"
      mode =
        case getVersion p of
          Nothing -> Matching
          _       -> NonMatching
      r = lookupResults mode p m
  pure
    . Set.map (\(c, pkg, _, _, _) -> Package c pkg Nothing Nothing Nothing)
    . keysSet
    $ r

currentInstalled ::
     (Reader Args :> es, Process :> es, FileSystem :> es)
  => Eff es (Either Running PackageSet)
currentInstalled = do
  (exitCode, inst, stdErr) <- runPquery installedArgs
  case exitCode of
    ExitSuccess ->
      case inst of
        (Just packageSet) -> pure . Right $ packageSet
        Nothing ->
          bStderr (prettyMessage "pquery output parsing failed")
            >> pure (Left Terminated)
    ExitFailure 127 ->
      bStderr
        (prettyMessage "Received exit code 127 from pquery. Is it installed")
        >> pure (Left Terminated)
    ExitFailure 1 -> pure (Left Terminated)
    ExitFailure ef ->
      bStderr
        (prettyMessage
           $ "pquery exited with unsuccessful code "
               ++ show ef
               ++ "\n"
               ++ stdErr)
        >> pure (Left Terminated)

tryInstall ::
     ( FileSystem :> es
     , Reader Args :> es
     , Process :> es
     , State St :> es
     , Time :> es
     )
  => Eff es (PrelimEmergeResult, String)
tryInstall = do
  pkg <- gets package
  bStderr . prettyTry $ "Trying " ++ (T.unpack . prettyPackage $ pkg)
  bStderr . prettyMessage $ "Checking for downgrades..."
  (exitCode, output) <- capturePortageOutput pkg
  case exitCode of
    ExitSuccess -> processIfNotDowngrade output
    _           -> currentTime >>= \time -> pure (ResolveFailed time, output)

processIfNotDowngrade ::
     ( Reader Args :> es
     , FileSystem :> es
     , Process :> es
     , State St :> es
     , Time :> es
     )
  => String
  -> Eff es (PrelimEmergeResult, String)
processIfNotDowngrade output = do
  downgrade <- checkForDowngrades output
  if downgrade
    then currentTime >>= \time -> pure (TriedToDowngrade time, output)
    else pure (PrelimEmergeSuccess, output)

install ::
     ( FileSystem :> es
     , State St :> es
     , Reader Args :> es
     , Process :> es
     , Time :> es
     , Concurrent :> es
     )
  => Eff es (EmergeResult, Running)
install = do
  pkg <- gets package
  bStderr . prettyMessage $ "Preliminary emerge run succeeded..."
  (exitCode, _, _) <- runEmerge ["--keep-going=y", "--color=n"] pkg
  time <- currentTime
  let result =
        if exitCode == ExitSuccess
          then EmergeSuccess time
          else BuildFailed time
  if exitCode == ExitSuccess
    then bStderr (successPackage pkg)
    else bStderr (failPackage pkg)
  (exitCode', _, _) <- runHaskellUpdater
  if exitCode' /= ExitSuccess
    then pure (result, Terminated)
    else pure (result, Running)

failed ::
     (State St :> es, Reader Args :> es, FileSystem :> es)
  => String
  -> PrelimEmergeResult
  -> Eff es ()
failed output result = do
  pkg <- gets package
  let (prefix, time, message, op) =
        case result of
          ResolveFailed t ->
            ( "resolve-failed-"
            , t
            , " failed while resolving with portage"
            , failedResolve)
          TriedToDowngrade t ->
            ("downgrade-", t, " tried to downgrade", hasDowngraded)
          _ -> undefined
  logPortageOutput time prefix pkg output
  bStderr . prettyPreMerge $ (T.unpack . prettyPackage $ pkg) ++ message
  modify (op time pkg)

logPortageOutput ::
     (FileSystem :> es, Reader Args :> es)
  => UTCTime
  -> String
  -> Package
  -> String
  -> Eff es ()
logPortageOutput time pathCircumstances pkg output = do
  let fullPath = tmpLogRoot ++ pathCircumstances ++ filePathPackage pkg
  stderr $ "Saving output to " ++ fullPath
  logOutput
    fullPath
    ((T.unpack . toDate $ time)
       ++ "\n"
       ++ (T.unpack . prettyPackage $ pkg)
       ++ "\n"
       ++ output)

totalStats ::
     (State St :> es, Reader Args :> es, Process :> es) => Eff es String
totalStats = do
  inst <- gets installed
  total <- asks getAllPackages
  let is = Set.size . Set.intersection total $ inst
      ts = Set.size total
      pc = (100 * is) `div` ts
  pure
    $ show is
        ++ " installed out of "
        ++ show ts
        ++ " total, "
        ++ show pc
        ++ "%."

terminate ::
     (FileSystem :> es, Reader Args :> es, State St :> es, Process :> es)
  => Eff es ()
terminate = do
  st <- get :: (State St :> es) => Eff es St
  bStdout . prettyPrintSt $ st
  stdout "\n"
  totalStats >>= stdout

capturePortageOutput ::
     (FileSystem :> es, Reader Args :> es, Process :> es)
  => Package
  -> Eff es (ExitCode, String)
capturePortageOutput pkg = do
  emerge <- asks getEmerge
  stderr
    (emerge ++ " " ++ unwords defaultEmergeArgs ++ " " ++ "--pretend --color=y")
  (exitCode, stdOut, stdErr) <- runEmerge ["--pretend", "--color=y"] pkg
  let output = stdOut ++ stdErr
  stderr ("pretend_return: " ++ output)
  pure (exitCode, output)

checkForDowngrades ::
     (FileSystem :> es, Reader Args :> es, State St :> es)
  => String
  -> Eff es Bool
checkForDowngrades portageOutput = do
  let result = parseDowngrades . BS.pack $ portageOutput
  case result of
    OK downgraded _ ->
      if downgraded
        then gets package >>= \pkg ->
               stderr
                 ("Downgrade detected: " ++ (T.unpack . prettyPackage $ pkg))
                 >> pure downgraded
        else stderr "No downgrade detected" >> pure downgraded
    _ -> error "generic parser error"

randomBuild ::
     ( FileSystem :> es
     , State St :> es
     , Reader Args :> es
     , Process :> es
     , Time :> es
     , Concurrent :> es
     )
  => Eff es Running
randomBuild = do
  u <- gets untried
  case uncons u of
    Nothing -> pure Terminated
    Just (pkg, ps) -> do
      modify (\st -> st {package = pkg})
      modify (addTried pkg)
      bStderr . prettyMessage
        $ show (length ps + 1) ++ " packages left to consider.\n"
      (preliminaryEmergeResult, preliminaryOutput) <- tryInstall
      r <-
        case preliminaryEmergeResult of
          PrelimEmergeSuccess -> do
            (emergeResult, running) <- install
            case emergeResult of
              BuildFailed t   -> modify (hasFailed t pkg)
              EmergeSuccess t -> modify (hasCompleted t pkg)
            pure running
          _ -> failed preliminaryOutput preliminaryEmergeResult >> pure Running
      if r == Running
        then do
          rawInstalled <- currentInstalled
          case rawInstalled of
            Left r' -> pure r'
            Right inst ->
              currentTime >>= \time ->
                modify (updateInstalled time ps inst)
                  >> totalStats
                  >>= stderr
                  >> pure r
        else pure r
