{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

label =
  $(do
      projectLabelClass <- newName "ProjectLabel"
      projectLabelMethod <- newName "projectLabel"
      a <- newName "a"
      addTopDecls
        [ ClassD [] projectLabelClass [PlainTV a BndrReq] []
            [ SigD projectLabelMethod
                (AppT (AppT ArrowT (VarT a)) (ConT ''String))
            ]
        , InstanceD Nothing [] (AppT (ConT projectLabelClass) (ConT ''Int))
            [ FunD projectLabelMethod
                [Clause [WildP] (NormalB (LitE (StringL "int"))) []]
            ]
        ]
      [| $(varE projectLabelMethod) (1 :: Int) |])

$(pure [])

describe :: ProjectLabel a => a -> String
describe = projectLabel

main :: IO ()
main = print (label, describe (1 :: Int))
