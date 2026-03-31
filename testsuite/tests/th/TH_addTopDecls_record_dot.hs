{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import TH_addTopDecls_ProjectRows

projects = [projectRows|select id, name from projects|]

names = map (.name) projects

$(pure [])

projectNames :: [Project] -> [String]
projectNames = map name

main :: IO ()
main = print (names, projectNames projects)
