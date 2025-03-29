{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module GHRB.Core
  ( -- St helpers
    buildEmptyState
  , -- State loggers
    failedResolve
  , hasDowngraded
  , hasCompleted
  , hasFailed
  , addTried
  , -- Package parser
    parsePackageList
  , parseDowngrades
  , -- Package type
    Package
  , -- prettyPrinters
    toDate
  , prettyPackage
  , filePathPackage
  , -- untried packages checker
    getUntried
  , updateInstalled
  ) where

import           Control.Applicative        (many, optional, some, (<|>))
import           Control.Monad              (void)
import qualified Data.ByteString            as BS (ByteString)
import qualified Data.ByteString.Char8      as BS (pack)
import qualified Data.HashSet               as Set (insert, member)
import           Data.List                  (partition)
import           Data.Text                  (Text)
import qualified Data.Text                  as T (append, cons, pack)
import           Data.Time.Clock            (UTCTime)
import           Data.Time.Format           (defaultTimeLocale, formatTime)
import           Data.Void                  (Void)
import           Distribution.Portage.Types (Category (Category),
                                             Package (Package),
                                             PkgName (PkgName), getCategory,
                                             getPkgName, getRepository, getSlot,
                                             getVersion, unwrapCategory,
                                             unwrapPkgName)
import           FlatParse.Basic            (Parser, Result (OK), char, eof,
                                             runParser, satisfy, string)
import           GHRB.Core.Types            (PackageSet, St (St), completed,
                                             downgrade, failed, installed,
                                             tried, unresolved, untried)

buildEmptyState :: St
buildEmptyState = St mempty mempty mempty mempty mempty mempty mempty undefined

parsePackageList :: String -> Maybe PackageSet
parsePackageList packageList =
  case runParser parsePackages . BS.pack $ packageList of
    OK packageSet _ -> Just packageSet
    _               -> Nothing

parsePackages :: Parser Void PackageSet
parsePackages = parsePackage <|> (eof >> return mempty)

parseANSI :: Parser Void ()
parseANSI = do
  $(string "\\[ESC")
  void $ many (satisfy (/= 'm'))
  $(char 'm')

parsePackage :: Parser Void PackageSet
parsePackage = do
  category <- many (satisfy (/= '/'))
  $(char '/')
  name <- many (satisfy (/= '\n'))
  void . optional $ $(char '\n')
  Set.insert
    Package
      { getCategory = Category category
      , getPkgName = PkgName name
      , getVersion = Nothing
      , getSlot = Nothing
      , getRepository = Nothing
      }
    <$> parsePackages

parseDowngrades :: BS.ByteString -> Result Void Bool
parseDowngrades = runParser parseDowngradeByLine

parseDowngradeByLine :: Parser Void Bool
parseDowngradeByLine =
  ($(char '[')
     >> some parseANSI
     >> $(string "ebuild")
     >> many (parseANSI <|> void (satisfy (`notElem` "D[]")))
     >> $(char 'D')
     >> many (parseANSI <|> void (satisfy (`notElem` "[]\n")))
     >> $(char ']')
     >> pure True)
    <|> (many (satisfy (/= '\n')) >> $(char '\n') >> parseDowngradeByLine)
    <|> (eof >> pure False)

toDate :: UTCTime -> Text
toDate = T.pack . formatTime defaultTimeLocale "%c"

prettyPackage :: Package -> Text
prettyPackage p =
  T.append (T.pack . unwrapCategory . getCategory $ p) . T.cons '/'
    $ T.pack (unwrapPkgName . getPkgName $ p)

filePathPackage :: Package -> String
filePathPackage p =
  (unwrapCategory . getCategory $ p)
    ++ '-'
    : (unwrapPkgName . getPkgName $ p)
    ++ ".log"

getUntried :: [Package] -> St -> St
getUntried packages st = st {untried = packages'}
  where
    packages' = filter (\p -> not (p `Set.member` tried st)) packages

failedResolve :: UTCTime -> Package -> St -> St
failedResolve t p st = st {unresolved = Set.insert (t, p) . unresolved $ st}

hasDowngraded :: UTCTime -> Package -> St -> St
hasDowngraded t p st = st {downgrade = Set.insert (t, p) . downgrade $ st}

hasCompleted :: UTCTime -> Package -> St -> St
hasCompleted t p st = st {completed = Set.insert (t, p) . completed $ st}

hasFailed :: UTCTime -> Package -> St -> St
hasFailed t p st = st {failed = Set.insert (t, p) . failed $ st}

addTried :: Package -> St -> St
addTried p st = st {tried = Set.insert p . tried $ st}

updateInstalled :: UTCTime -> [Package] -> PackageSet -> St -> St
updateInstalled time ps inst st = st'
  where
    (comp, untried') = partition (`Set.member` inst) ps
    st' =
      foldr (hasCompleted time) st {untried = untried', installed = inst} comp
