--------------------------------------------------------------------------------
-- | Takes program input, and passes it to Output module.
module Interpreter where

import Language

interpret :: Program -> Either Err Memory
interpret = undefined