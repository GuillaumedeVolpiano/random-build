{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module GHRB.Core.Types
  ( AttemptSets
  , Stdout
  , Stderr
  , PqueryPath
  , EmergePath
  , HaskellUpdaterPath
  , RevdepScannerPath
  , --Console arguments
    Args(Args)
  , getPquery
  , getEmerge
  , getHU
  , getPackage
  , getOutputMode
  , getErrMode
  , getAllPackages
  , args
  , -- The process state
    St(St)
  , completed
  , failed
  , downgrade
  , installed
  , tried
  , unresolved
  , untried
  , package
  , -- OutputMode
    Output(Std, DevNull, OutFile)
  , PackageSet
  , -- Running state
    Running(Running, Terminated)
  , PrelimEmergeResult(PrelimEmergeSuccess, ResolveFailed,
                   TriedToDowngrade)
  , EmergeResult(BuildFailed, EmergeSuccess)
  , prettyPrintSt
  , success
  , failure
  , preMergeFailure
  , message
  , try
  , successPackage
  , failPackage
  ) where

import           Control.Applicative           (optional, (<|>))
import qualified Data.ByteString.Lazy          as BL (ByteString)
import           Data.HashSet                  (HashSet)
import qualified Data.HashSet                  as Set (map, size, toList)
import           Data.Parsable                 (runParsable)
import           Data.Time.Clock.Compat        (UTCTime)
import           Distribution.Portage.Types    (Package, getCategory,
                                                getPkgName, unwrapCategory,
                                                unwrapPkgName)
import           Options.Applicative           (Parser, flag', help, long,
                                                metavar, short, strOption,
                                                value)
import           Prettyprinter                 (Doc, SimpleDocStream, annotate,
                                                defaultLayoutOptions, hardline,
                                                layoutPretty, pretty, vsep,
                                                (<+>))
import           Prettyprinter.Render.Terminal (AnsiStyle,
                                                Color (Cyan, Green, Magenta, Red, Yellow),
                                                color)

type Set = HashSet

type EmergePath = FilePath

type PqueryPath = FilePath

type HaskellUpdaterPath = FilePath

type RevdepScannerPath = FilePath

data PrelimEmergeResult
  = ResolveFailed UTCTime
  | TriedToDowngrade UTCTime
  | PrelimEmergeSuccess
  deriving (Show, Eq, Ord)

data EmergeResult
  = BuildFailed UTCTime
  | EmergeSuccess UTCTime
  deriving (Show, Eq, Ord)

type ResolveFailedAttempts = Set (UTCTime, Package)

type TriedToDowngradeAttempts = Set (UTCTime, Package)

type BuildFailedAttempts = Set (UTCTime, Package)

type EmergeSuccessAttempts = Set (UTCTime, Package)

type AttemptSets
  = ( ResolveFailedAttempts
    , TriedToDowngradeAttempts
    , BuildFailedAttempts
    , EmergeSuccessAttempts)

type Stdout = BL.ByteString

type Stderr = BL.ByteString

-- | A monad class to output messages. Minimum complete definition stdout,
-- readProcessWithExitCode, bStdErr || stderr, logOutput
data Args = Args
  { getPquery      :: String
  , getEmerge      :: String
  , getHU          :: String
  , getPackage     :: Maybe Package
  , getOutputMode  :: Output
  , getErrMode     :: Output
  , getAllPackages :: PackageSet
  }

type PackageSet = Set Package

success :: Doc AnsiStyle -> Doc AnsiStyle
success = annotate (color Green)

failure :: Doc AnsiStyle -> Doc AnsiStyle
failure = annotate (color Red)

preMergeFailure :: Doc AnsiStyle -> Doc AnsiStyle
preMergeFailure = annotate (color Yellow)

message :: Doc AnsiStyle -> Doc AnsiStyle
message = annotate (color Magenta)

try :: Doc AnsiStyle -> Doc AnsiStyle
try = annotate (color Cyan)

-- | Our current build state
data St = St
  { completed  :: EmergeSuccessAttempts
  , failed     :: BuildFailedAttempts
  , downgrade  :: TriedToDowngradeAttempts
  , unresolved :: ResolveFailedAttempts
  , tried      :: PackageSet
  , installed  :: PackageSet
  , untried    :: [Package]
  , package    :: Package
  } deriving (Eq)

prettyPrintSt :: St -> SimpleDocStream AnsiStyle
prettyPrintSt = layoutPretty defaultLayoutOptions . prettySt

prettySt :: St -> Doc AnsiStyle
prettySt st =
  hardline
    <> pretty "--------"
    <> hardline
    <> hardline
    <> pretty "Results:"
    <> hardline
    <> hardline
    <> pretty "Completed:"
    <> hardline
    <> hardline
    <> success (prettyPrintSet . Set.map snd $ completed st)
    <> hardline
    <> hardline
    <> pretty "Failed:"
    <> hardline
    <> hardline
    <> failure (prettyPrintSet . Set.map snd $ failed st)
    <> hardline
    <> hardline
    <> pretty "These packages tried to downgrade:"
    <> hardline
    <> hardline
    <> preMergeFailure (prettyPrintSet . Set.map snd $ downgrade st)
    <> hardline
    <> hardline
    <> pretty "These packages failed to resolve:"
    <> hardline
    <> hardline
    <> preMergeFailure (prettyPrintSet . Set.map snd $ unresolved st)
    <> hardline
    <> hardline
    <> pretty "Statistics:"
    <> hardline
    <> hardline
    <> pretty "success:"
    <+> success (pretty sc) <> hardline <> pretty "failures:"
    <+> failure (pretty sf) <> hardline <> pretty "downgrades:"
    <+> preMergeFailure (pretty sd) <> hardline <> pretty "unresolved:"
    <+> preMergeFailure (pretty sur) <> hardline <> pretty "untried:"
    <+> pretty sun <> hardline <> pretty "success rate:"
    <+> success (pretty sr <> pretty '%')
  where
    sc = Set.size . completed $ st
    sf = Set.size . failed $ st
    sd = Set.size . downgrade $ st
    sur = Set.size . unresolved $ st
    sun = length . untried $ st
    sr =
      if sc + sf + sd + sur == 0
        then 0
        else (sc * 100) `div` (sc + sf + sd + sur)

prettyPrintSet :: PackageSet -> Doc ann
prettyPrintSet = vsep . map prettyPrintPackage . Set.toList

prettyPrintPackage :: Package -> Doc ann
prettyPrintPackage p =
  pretty (unwrapCategory . getCategory $ p)
    <> pretty '/'
    <> pretty (unwrapPkgName . getPkgName $ p)

successPackage :: Package -> SimpleDocStream AnsiStyle
successPackage p =
  layoutPretty defaultLayoutOptions . success
    $ pretty "***" <+> prettyPrintPackage p <> pretty ": Success!"

failPackage :: Package -> SimpleDocStream AnsiStyle
failPackage p =
  layoutPretty defaultLayoutOptions . failure
    $ pretty "***" <+> prettyPrintPackage p <> pretty ": Build failed!"

data Output
  = Std
  | DevNull
  | OutFile String

-- | An ADT to report whether the program should have terminated
data Running
  = Terminated
  | Running
  deriving (Eq)

args :: Parser Args
args =
  Args
    <$> strOption
          (long "pquery"
             <> short 'p'
             <> metavar "Filepath"
             <> value ""
             <> help "Path to the eix binary")
    <*> strOption
          (long "emerge"
             <> short 'e'
             <> metavar "Filepath"
             <> value ""
             <> help "Path to the emerge binary")
    <*> strOption
          (long "haskell-updater"
             <> short 'u'
             <> metavar "Filepath"
             <> value ""
             <> help "Path to the haskell-updater binary")
    <*> (parsePackage
           <$> strOption
                 (long "package"
                    <> short 'a'
                    <> metavar "Package and potential version"
                    <> value ""
                    <> help
                         "Use revdep-scanner with specified package and version instead \
                        \of the whole repository as the initial list"))
    <*> ((\case
            Nothing -> Std
            Just a -> a)
           <$> optional (outputFile <|> stdOut))
    <*> ((\case
            Nothing -> Std
            Just a -> a)
           <$> optional (logFile <|> stdErr <|> quiet))
    <*> pure mempty

parsePackage :: String -> Maybe Package
parsePackage s
  | null s = Nothing
  | otherwise =
    case runParsable "package argument" s of
      Left _ ->
        error "At least one full package name (and optional version) required"
      Right p -> Just p

outputFile :: Parser Output
outputFile =
  OutFile
    <$> strOption
          (long "out"
             <> short 'o'
             <> metavar "Filepath"
             <> help "File to output to")

stdOut :: Parser Output
stdOut = flag' Std (long "stdout" <> short 's' <> help "Output to stdout")

logFile :: Parser Output
logFile =
  OutFile
    <$> strOption
          (long "log"
             <> short 'l'
             <> metavar "Filepath"
             <> help "File to log the process to")

stdErr :: Parser Output
stdErr =
  flag' Std (long "stderr" <> short 'e' <> help "Output the log to stderr")

quiet :: Parser Output
quiet = flag' DevNull (long "quiet" <> short 'q' <> help "Be less verbose")
