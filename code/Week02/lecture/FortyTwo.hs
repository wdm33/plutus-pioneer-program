{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}

module FortyTwo where

import qualified Plutus.V2.Ledger.Api as PlutusV2
import           PlutusTx             (compile)
import           PlutusTx.Prelude     (Bool, Integer, traceIfFalse, ($), (==))
import           Prelude              (IO)
import           Utilities            (wrap, writeValidatorToFile)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

-- This validator succeeds only if the redeemer is 42
--                  Datum         Redeemer     ScriptContext
mk42Validator :: () -> Integer -> PlutusV2.ScriptContext -> Bool
mk42Validator _ r _ = traceIfFalse "expected 42" $ r == 42

{-# INLINABLE mk42Validator #-}

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| wrap mk42Validator ||])

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveVal :: IO ()
saveVal = writeValidatorToFile "./assets/fortytwo.plutus" validator
