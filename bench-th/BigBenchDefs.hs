{-# LANGUAGE TemplateHaskell #-}
module BigBenchDefs where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Char (toLower)

-- Helper that generates a record-like data type with N fields
mkRecord :: String -> Int -> Q [Dec]
mkRecord name n = do
  let fields = [ (mkName (map toLower name ++ "_field" ++ show i), Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Int) | i <- [1..n] ]
  let con = RecC (mkName name) fields
  pure [DataD [] (mkName name) [] Nothing [con] []]
