module Lib
    ( someFunc
    ) where

import Language
import Input
import Parser
import Interpreter
import Output

someFunc :: IO ()
someFunc = putStrLn "someFunc"
