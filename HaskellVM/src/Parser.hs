--------------------------------------------------------------------------------
-- | Takes error-free string input, and hands a program to the Interpreter.
module Parser where

import Language

--------------------------------------------------------------------------------

-- | TODO: Use zip in order to pair lines with line numbers, e.g. zip file [0..]


-- | Program :: Zipper (LineNo, Instruction)
parse :: String -> Either Err Program
parse = undefined
