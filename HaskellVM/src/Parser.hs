--------------------------------------------------------------------------------
-- | Takes error-free string input, and hands a program to the Interpreter.
module Parser where

import Language

parse :: String -> Either Err Program
parse = undefined