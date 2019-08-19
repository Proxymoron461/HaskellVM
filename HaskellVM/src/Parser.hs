--------------------------------------------------------------------------------
-- | Takes error-free string input, and hands a program to the Interpreter.
module Parser where

import Language
import qualified Data.ByteString as BS

--------------------------------------------------------------------------------

-- | TODO: Use zip in order to pair lines with line numbers, e.g. zip file [0..]


-- | Program :: Zipper (LineNo, Instruction)
parse :: String -> Either Err Program
parse x = Left "parse not defined yet"
    where
        xs = lines x

-- | Efficient version of parse which will read the file in a strict manner.        
parse' :: BS.ByteString -> Either Err Program
parse' = undefined