{-# LANGUAGE TemplateHaskell #-}
module AesonBench where

import Data.Aeson.TH
import AesonTypes

-- 20 deriveJSON splices — each generates both ToJSON and FromJSON instances
-- This is the typical real-world TH usage pattern

$(deriveJSON defaultOptions ''User)
$(deriveJSON defaultOptions ''Address)
$(deriveJSON defaultOptions ''Product)
$(deriveJSON defaultOptions ''OrderItem)
$(deriveJSON defaultOptions ''Order)
$(deriveJSON defaultOptions ''Company)
$(deriveJSON defaultOptions ''Employee)
$(deriveJSON defaultOptions ''Invoice)
$(deriveJSON defaultOptions ''Config)
$(deriveJSON defaultOptions ''Event)
$(deriveJSON defaultOptions ''ApiResponse)
$(deriveJSON defaultOptions ''Session)
$(deriveJSON defaultOptions ''Notification)
$(deriveJSON defaultOptions ''Permission)
$(deriveJSON defaultOptions ''AuditLog)
$(deriveJSON defaultOptions ''Metric)
$(deriveJSON defaultOptions ''Report)
$(deriveJSON defaultOptions ''Task)
$(deriveJSON defaultOptions ''Comment)
$(deriveJSON defaultOptions ''Tag)
$(deriveJSON defaultOptions ''Webhook)
