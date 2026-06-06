{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

$(do
    addTopDecls [TySynD (mkName "NonExact") [] (ConT ''Int)]
    pure [])
