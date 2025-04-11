{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeOperators    #-}

module GHRB.IO.Utils
  ( bStderr
  , stderr
  , bStdout
  , stdout
  , logOutput
  , getArgs
  ) where

import           Control.Applicative                     ((<**>))
import           Control.Monad                           (void)
import           Control.Monad.IO.Class                  (liftIO)
import qualified Data.ByteString.Lazy.Char8              as BL (pack)
import qualified Data.Text.Lazy.Encoding                 as TL (encodeUtf8)
import           Effectful                               (Eff, IOE, (:>))
import           Effectful.Environment                   (Environment, setEnv)
import           Effectful.FileSystem                    (FileSystem,
                                                          findExecutable)
import qualified Effectful.FileSystem.IO                 as IO (stderr, stdout)
import qualified Effectful.FileSystem.IO.ByteString.Lazy as BL (appendFile,
                                                                hPutStrLn,
                                                                writeFile)
import           Effectful.Reader.Static                 (Reader, asks)
import           GHRB.Core.Types                         (Args, EmergePath,
                                                          HaskellUpdaterPath,
                                                          Output (DevNull, OutFile, Std),
                                                          PqueryPath,
                                                          args, getEmerge,
                                                          getErrMode, getHU,
                                                          getOutputMode,
                                                          getPquery)
import           Options.Applicative                     (execParser, fullDesc,
                                                          helper, info,
                                                          progDesc)
import           Prettyprinter                           (Pretty,
                                                          SimpleDocStream,
                                                          defaultLayoutOptions,
                                                          layoutPretty, pretty,
                                                          unAnnotateS)
import           Prettyprinter.Render.Terminal           (AnsiStyle)
import qualified Prettyprinter.Render.Terminal           as T (renderLazy)
import qualified Prettyprinter.Render.Text               as TL (renderLazy)
import           System.Exit                             (die)

bStdout ::
     (FileSystem :> es, Reader Args :> es)
  => SimpleDocStream AnsiStyle
  -> Eff es ()
bStdout message = do
  outmode <- asks getOutputMode
  case outmode of
    Std -> BL.hPutStrLn IO.stdout . TL.encodeUtf8 . T.renderLazy $ message
    OutFile fp ->
      BL.appendFile fp . TL.encodeUtf8 . TL.renderLazy . unAnnotateS $ message
    DevNull -> pure ()

stdout :: (FileSystem :> es, Reader Args :> es, Pretty a) => a -> Eff es ()
stdout = bStdout . layoutPretty defaultLayoutOptions . pretty

logOutput :: (FileSystem :> es) => FilePath -> String -> Eff es ()
logOutput filepath = BL.writeFile filepath . BL.pack

stderr :: (FileSystem :> es, Reader Args :> es, Pretty a) => a -> Eff es ()
stderr = bStderr . layoutPretty defaultLayoutOptions . pretty

bStderr ::
     (FileSystem :> es, Reader Args :> es)
  => SimpleDocStream AnsiStyle
  -> Eff es ()
bStderr message = do
  errmode <- asks getErrMode
  case errmode of
    DevNull -> pure ()
    Std -> BL.hPutStrLn IO.stderr . TL.encodeUtf8 . T.renderLazy $ message
    (OutFile fp) ->
      BL.appendFile fp . TL.encodeUtf8 . TL.renderLazy . unAnnotateS $ message

getArgs :: (IOE :> es, FileSystem :> es, Environment :> es) => Eff es Args
getArgs = do
  args' <-
    liftIO
      $ execParser . info (args <**> helper)
      $ (fullDesc
           <> progDesc
                "A utility to repeatedly randomly build haskell packages from ::haskell")
  pquery <-
    if null (getPquery args')
      then pqueryPath
      else pure . getPquery $ args'
  emerge <-
    if null (getEmerge args')
      then emergePath
      else pure . getEmerge $ args'
  hu <-
    if null (getHU args')
      then haskellUpdaterPath
      else pure . getHU $ args'
  pure
    args' {getPquery = pquery, getEmerge = emerge, getHU = hu}

emergePath ::
     (IOE :> es, FileSystem :> es, Environment :> es) => Eff es EmergePath
emergePath =
  findExecutable "emerge" >>= \case
    Just p -> do
      void . setEnv "FEATURES" $ "-getbinpkg"
      pure p
    Nothing ->
      liftIO
        $ die
            "Could not find emerge executable. Install sys-apps/portage first."

-- | Find the path to the @pquery@ executable or throw an error. Caches the
--   result in the case of a success.
pqueryPath ::
     (FileSystem :> es, Environment :> es, IOE :> es) => Eff es PqueryPath
pqueryPath =
  findExecutable "pquery" >>= \case
    Just p -> pure p
    Nothing ->
      liftIO
        $ die
            "Could not find pquery executable. Install sys-apps/pkgcore first."

-- | Find the path to the @haskell-updater@ executable or throw an error.
--   Caches the result in the case of a success. Sets
--   @FEATURES="-getbinpkg"@ to avoid it interfering with this utility.
haskellUpdaterPath ::
     (FileSystem :> es, Environment :> es, IOE :> es)
  => Eff es HaskellUpdaterPath
haskellUpdaterPath =
  findExecutable "haskell-updater" >>= \case
    Just p -> do
      setEnv "FEATURES" "-getbinpkg"
      pure p
    Nothing ->
      liftIO
        $ die
            "Could not find haskell-updater executable. \
           \Install app-admin/haskell-updater first."
