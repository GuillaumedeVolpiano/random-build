{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Repeatedly tries to install a random package from ::haskell (that is not
-- already installed and has at least one unmasked version)
--
-- - Notes successes, errors, packages that try to downgrade, and packages that
--   fail to resolve
-- - Rudimentary logger to files: /tmp/random-pkg-*
-- - Runs `haskell-updater` after every attempt -- Aborts script if it fails
-- - Aborts on on SIGINT (Ctrl+C)
-- Based on hololeap's build-random-haskell-pkgs.bash
module Main where

import           Control.Monad                 (when)
import           Control.Monad.IO.Class        (liftIO)
import           Data.HashSet                  (toList)
import           Effectful                     (Eff, runEff, (:>))
import           Effectful.Environment         (runEnvironment)
import           Effectful.Exception           (finally)
import           Effectful.FileSystem          (FileSystem, runFileSystem)
import           Effectful.Process             (Process, runProcess)
import           Effectful.Reader.Static       (Reader, runReader)
import           Effectful.State.Static.Shared (State, evalState)
import           Effectful.Time                (Time, runTime)
import Effectful.Concurrent (Concurrent, runConcurrent)
import           GHRB.Core                     (buildEmptyState)
import           GHRB.Core.Types               (Args, Running (Running), St,
                                                getAllPackages, getPquery,
                                                untried, getPackage)
import           GHRB.IO                       (allPackages, currentUntried,
                                                randomBuild, terminate)
import           GHRB.IO.Utils                 (getArgs, stderr)
import           List.Shuffle                  (shuffleIO)

import Debug.Trace

builder ::
     ( FileSystem :> es
     , Process :> es
     , State St :> es
     , Reader Args :> es
     , Time :> es
     , Concurrent :> es
     )
  => Eff es ()
builder = do
  running <- randomBuild
  when (running == Running) (randomBuild >> builder)

main :: IO ()
main =
  runEff . runEnvironment . runFileSystem $ do
    let initialState = buildEmptyState
    args <- getArgs
    runProcess $ do
      rawAp <- allPackages (getPackage args). getPquery $ args
      case rawAp of
        Nothing ->
          runReader args $ stderr "Could not get list of all available packages"
        Just ap ->
          runReader (args {getAllPackages = ap}) $ do
            rawNotInstalled <- currentUntried
            case rawNotInstalled of
              Left _ -> stderr "Could not get list of uninstalled packages"
              Right set -> do
                untried' <- liftIO . shuffleIO . toList $ set
                let state = initialState {untried = untried'}
                traceShow untried' runConcurrent . runTime . evalState state $ finally builder terminate
