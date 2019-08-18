module Lib
    ( entry
    ) where

import Language
import Parser
import Interpreter
import Output
import System.Environment (getArgs)

entry :: IO ()
entry = input

-- | Take in user's command line input for the file name, bind it to a variable,
-- and hand that to the parser.
-- | TODO: catch IOExceptions
input :: IO ()
input = do
            (file:fs) <- getArgs
                -- | Returns IO [String], use monads to bind the [String] to (file:fs)
            code <- readFile file 
                -- | Use first command line argument (the file name) to read the file
                -- and bind it to code, where code :: String.
                -- | Runs in the current directory, so the file available should
                -- exist in the current directory. (What I mean is that if you
                -- are in /Haskell/HaskellVM and you run stack run mine.txt, then
                -- mine.txt needs to be in /Haskell/HaskellVM to get found).
                -- | FIXME: currently, does not catch IOExceptions.
            either putStrLn putStrLn (parse code >>= interpret >>= output)
                -- | Whether output is memory or error, print to screen!
