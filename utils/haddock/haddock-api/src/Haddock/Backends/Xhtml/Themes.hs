-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Haddock.Backends.Html.Themes
-- Copyright   :  (c) Mark Lentczner 2010
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
module Haddock.Backends.Xhtml.Themes
  ( Themes
  , getThemes
  , cssFiles
  , styleSheet
  )
where

import Control.Monad (liftM)
import Data.Char (toLower)
import Data.Either (lefts, rights)
import Data.List (nub)
import Data.Maybe (isJust, listToMaybe)
import GHC.Data.OsPath (OsPath, unsafeDecodeUtf, unsafeEncodeUtf, (</>), (<.>))
import qualified System.OsPath as OsPath
import System.Directory.OsPath
import Text.XHtml hiding (name, p, quote, title, (</>))
import qualified Text.XHtml as XHtml

import Haddock.Backends.Xhtml.Types (BaseURL, withBaseURL)
import Haddock.Options

--------------------------------------------------------------------------------

-- * CSS Themes

--------------------------------------------------------------------------------

data Theme = Theme
  { themeName :: String
  , themeHref :: String
  , themeFiles :: [OsPath]
  }

type Themes = [Theme]

type PossibleTheme = Either String Theme
type PossibleThemes = Either String Themes

-- | Find a theme by name (case insensitive match)
findTheme :: String -> Themes -> Maybe Theme
findTheme s = listToMaybe . filter ((== ls) . lower . themeName)
  where
    lower = map toLower
    ls = lower s

-- | Standard theme used by default
standardTheme :: OsPath -> IO PossibleThemes
standardTheme libDir = liftM (liftEither (take 1)) (defaultThemes libDir)

-- | Default themes that are part of Haddock; added with @--built-in-themes@
-- The first theme in this list is considered the standard theme.
-- Themes are "discovered" by scanning the html sub-dir of the libDir,
-- and looking for directories with the extension .theme or .std-theme.
-- The later is, obviously, the standard theme.
defaultThemes :: OsPath -> IO PossibleThemes
defaultThemes libDir = do
  themeDirs <- getDirectoryItems (libDir </> unsafeEncodeUtf "html")
  themes <- mapM directoryTheme $ discoverThemes themeDirs
  return $ sequenceEither themes
  where
    discoverThemes paths =
      filterExt (unsafeEncodeUtf ".std-theme") paths ++ filterExt (unsafeEncodeUtf ".theme") paths
    filterExt ext = filter ((== ext) . OsPath.takeExtension)

-- | Build a theme from a single .css file
singleFileTheme :: OsPath -> IO PossibleTheme
singleFileTheme path =
  if isCssFilePath path
    then retRight $ Theme name file [path]
    else errMessage "File extension isn't .css" path
  where
    name = unsafeDecodeUtf (OsPath.takeBaseName path)
    file = unsafeDecodeUtf (OsPath.takeFileName path)

-- | Build a theme from a directory
directoryTheme :: OsPath -> IO PossibleTheme
directoryTheme path = do
  items <- getDirectoryItems path
  case filter isCssFilePath items of
    [cf] -> retRight $ Theme (unsafeDecodeUtf (OsPath.takeBaseName path)) (unsafeDecodeUtf (OsPath.takeFileName cf)) items
    [] -> errMessage "No .css file in theme directory" path
    _ -> errMessage "More than one .css file in theme directory" path

-- | Check if we have a built in theme
doesBuiltInExist :: IO PossibleThemes -> String -> IO Bool
doesBuiltInExist pts s = fmap (either (const False) test) pts
  where
    test = isJust . findTheme s

-- | Find a built in theme
builtInTheme :: IO PossibleThemes -> String -> IO PossibleTheme
builtInTheme pts s = either Left fetch <$> pts
  where
    fetch = maybe (Left ("Unknown theme: " ++ s)) Right . findTheme s

--------------------------------------------------------------------------------

-- * CSS Theme Arguments

--------------------------------------------------------------------------------

-- | Process input flags for CSS Theme arguments
getThemes :: OsPath -> [Flag] -> IO PossibleThemes
getThemes libDir flags =
  liftM concatEither (mapM themeFlag flags) >>= someTheme
  where
    themeFlag :: Flag -> IO (Either String Themes)
    themeFlag (Flag_CSS path) = (liftM . liftEither) (: []) (theme (unsafeEncodeUtf path))
    themeFlag (Flag_BuiltInThemes) = builtIns
    themeFlag _ = retRight []

    theme :: OsPath -> IO PossibleTheme
    theme path =
      pick
        path
        [ (doesFileExist, singleFileTheme)
        , (doesDirectoryExist, directoryTheme)
        , (doesBuiltInExist builtIns, builtInTheme builtIns)
        ]
        "Theme not found"

    pick
      :: OsPath
      -> [(OsPath -> IO Bool, OsPath -> IO PossibleTheme)]
      -> String
      -> IO PossibleTheme
    pick path [] msg = errMessage msg path
    pick path ((test, build) : opts) msg = do
      pass <- test path
      if pass then build path else pick path opts msg

    someTheme :: Either String Themes -> IO (Either String Themes)
    someTheme (Right []) = standardTheme libDir
    someTheme est = return est

    builtIns = defaultThemes libDir

errMessage :: String -> OsPath -> IO (Either String a)
errMessage msg path = return (Left msg')
  where
    msg' = "Error: " ++ msg ++ ": \"" ++ unsafeDecodeUtf path ++ "\"\n"

retRight :: a -> IO (Either String a)
retRight = return . Right

--------------------------------------------------------------------------------

-- * File Utilities

--------------------------------------------------------------------------------

getDirectoryItems :: OsPath -> IO [OsPath]
getDirectoryItems path =
  map (path </>) . filter notDot <$> listDirectory path
  where
    notDot s = s /= unsafeEncodeUtf "." && s /= unsafeEncodeUtf ".."

isCssFilePath :: OsPath -> Bool
isCssFilePath path = OsPath.takeExtension path == unsafeEncodeUtf ".css"

--------------------------------------------------------------------------------

-- * Style Sheet Utilities

--------------------------------------------------------------------------------

cssFiles :: Themes -> [String]
cssFiles ts = nub $ concatMap themeFiles ts

styleSheet :: BaseURL -> Themes -> Html
styleSheet base_url ts = toHtml $ zipWith mkLink rels ts
  where
    rels = "stylesheet" : repeat "alternate stylesheet"
    mkLink aRel t =
      thelink
        ! [ href (withBaseURL base_url (themeHref t))
          , rel aRel
          , thetype "text/css"
          , XHtml.title (themeName t)
          ]
        << noHtml

--------------------------------------------------------------------------------

-- * Either Utilities

--------------------------------------------------------------------------------

-- These three routines are here because Haddock does not have access to the
-- Control.Monad.Error module which supplies the Functor and Monad instances
-- for Either String.

sequenceEither :: [Either a b] -> Either a [b]
sequenceEither es = maybe (Right $ rights es) Left (listToMaybe (lefts es))

liftEither :: (b -> c) -> Either a b -> Either a c
liftEither f = either Left (Right . f)

concatEither :: [Either a [b]] -> Either a [b]
concatEither = liftEither concat . sequenceEither
