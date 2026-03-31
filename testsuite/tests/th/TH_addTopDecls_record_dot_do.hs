{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

import TH_addTopDecls_ProjectRows

main :: IO ()
main = do
  projects <- [projectRowsIO|select id, name from projects|]
  let names = map (.name) projects
      projectDescriptions =
        map (\project -> show project.projectId ++ ":" ++ project.name) projects
  print (names, projectDescriptions)
