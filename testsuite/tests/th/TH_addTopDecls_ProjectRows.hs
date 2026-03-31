{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module TH_addTopDecls_ProjectRows (projectRows, projectRowsIO) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

projectRows :: QuasiQuoter
projectRows = mkProjectRowsQuoter id

projectRowsIO :: QuasiQuoter
projectRowsIO = mkProjectRowsQuoter (\rows -> [| pure $rows |])

mkProjectRowsQuoter :: (Q Exp -> Q Exp) -> QuasiQuoter
mkProjectRowsQuoter wrap = QuasiQuoter
  { quoteExp = \_ -> do
      projectType <- newName "Project"
      projectCon <- newName "Project"
      projectId <- newName "projectId"
      projectName <- newName "name"
      let strictness = Bang NoSourceUnpackedness NoSourceStrictness
      addTopDecls
        [ DataD
            []
            projectType
            []
            Nothing
            [ RecC projectCon
                [ (projectId, strictness, ConT ''Int)
                , (projectName, strictness, ConT ''String)
                ]
            ]
            []
        ]
      wrap $
        listE
          [ appsE [conE projectCon, litE (integerL 1), litE (stringL "alpha")]
          , appsE [conE projectCon, litE (integerL 2), litE (stringL "beta")]
          ]
  , quotePat = unsupported "patterns"
  , quoteType = unsupported "types"
  , quoteDec = unsupported "declarations"
  }
  where
    unsupported thing _ = fail ("projectRows does not support " ++ thing)
