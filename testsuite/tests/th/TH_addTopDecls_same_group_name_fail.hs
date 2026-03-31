{-# LANGUAGE QuasiQuotes #-}

import TH_addTopDecls_ProjectRows

projects = [projectRows|select id, name from projects|]

badNames = map name projects
