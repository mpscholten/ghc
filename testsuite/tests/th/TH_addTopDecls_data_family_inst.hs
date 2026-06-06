{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

value =
  $(do
      boxFamily <- newName "Box"
      intBoxCon <- newName "IntBox"
      boxValueField <- newName "boxValue"
      a <- newName "a"
      let strictness = Bang NoSourceUnpackedness NoSourceStrictness
      addTopDecls
        [ DataFamilyD boxFamily [PlainTV a BndrReq] Nothing
        , DataInstD [] Nothing (AppT (ConT boxFamily) (ConT ''Int)) Nothing
            [RecC intBoxCon [(boxValueField, strictness, ConT ''Int)]]
            []
        ]
      [| $(varE boxValueField) $(appsE [conE intBoxCon, litE (integerL 7)]) |])

$(pure [])

extract :: Box Int -> Int
extract = boxValue

main :: IO ()
main = print (value, extract (IntBox 8))
