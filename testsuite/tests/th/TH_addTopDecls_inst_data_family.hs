{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

value =
  $(do
      bucketClass <- newName "BucketClass"
      bucketFamily <- newName "Bucket"
      intBucketCon <- newName "IntBucket"
      bucketValueField <- newName "bucketValue"
      a <- newName "a"
      let strictness = Bang NoSourceUnpackedness NoSourceStrictness
      addTopDecls
        [ ClassD [] bucketClass [PlainTV a BndrReq] []
            [DataFamilyD bucketFamily [PlainTV a BndrReq] Nothing]
        , InstanceD Nothing [] (AppT (ConT bucketClass) (ConT ''Int))
            [ DataInstD [] Nothing (AppT (ConT bucketFamily) (ConT ''Int)) Nothing
                [RecC intBucketCon [(bucketValueField, strictness, ConT ''Int)]]
                []
            ]
        ]
      [| $(varE bucketValueField) $(appsE [conE intBucketCon, litE (integerL 3)]) |])

$(pure [])

extract :: Bucket Int -> Int
extract = bucketValue

main :: IO ()
main = print (value, extract (IntBucket 4))
