{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

projectIds =
  $(do
      projectIdType <- newName "ProjectId"
      projectIdCon <- newName "ProjectId"
      projectIdsType <- newName "ProjectIds"
      let strictness = Bang NoSourceUnpackedness NoSourceStrictness
      addTopDecls
        [ NewtypeD [] projectIdType [] Nothing
            (NormalC projectIdCon [(strictness, ConT ''Int)])
            []
        , TySynD projectIdsType [] (AppT ListT (ConT projectIdType))
        ]
      sigE
        (listE
          [ appsE [conE projectIdCon, litE (integerL 3)]
          , appsE [conE projectIdCon, litE (integerL 4)]
          ])
        (conT projectIdsType))

$(pure [])

total :: ProjectIds -> Int
total = sum . map (\(ProjectId n) -> n)

main :: IO ()
main = print (total projectIds)
