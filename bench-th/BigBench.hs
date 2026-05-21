{-# LANGUAGE TemplateHaskell #-}
module BigBench where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import BigBenchDefs

-- 20 splices generating record types
$(mkRecord "Record1" 5)
$(mkRecord "Record2" 5)
$(mkRecord "Record3" 5)
$(mkRecord "Record4" 5)
$(mkRecord "Record5" 5)
$(mkRecord "Record6" 5)
$(mkRecord "Record7" 5)
$(mkRecord "Record8" 5)
$(mkRecord "Record9" 5)
$(mkRecord "Record10" 5)
$(mkRecord "Record11" 5)
$(mkRecord "Record12" 5)
$(mkRecord "Record13" 5)
$(mkRecord "Record14" 5)
$(mkRecord "Record15" 5)
$(mkRecord "Record16" 5)
$(mkRecord "Record17" 5)
$(mkRecord "Record18" 5)
$(mkRecord "Record19" 5)
$(mkRecord "Record20" 5)
