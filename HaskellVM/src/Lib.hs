module Lib
    ( entry
    ) where

import Language
import Parser
import Interpreter
import Output
import System.Environment (getArgs)
import System.IO.Error
import Control.Exception (catch)
import System.Directory (getCurrentDirectory)


entry :: IO ()
entry = input


-- | Take in user's command line input for the file name, bind it to a variable,
-- and hand that to the parser.
input :: IO ()
input = do
            (fileName:fs) <- getArgs
                -- | Returns IO [String], use monads to bind the [String] to (file:fs)
                -- so that the first command line argument is bound to fileName.
            fileContents <- catchReadFile fileName
                -- | Use first command line argument (the file name) to read the file
                -- and bind it to code, where code :: String.
                -- | Runs in the current directory, so the file available should
                -- exist in the current directory. (What I mean is that if you
                -- are in /Haskell/HaskellVM and you run stack run mine.txt, then
                -- mine.txt needs to be in /Haskell/HaskellVM to get found).
            either putStrLn putStrLn (parse fileContents >>= interpret >>= output)
                -- | Whether output is memory or error, print to screen!


-- | Given a FilePath (a String) and an IOError, returns an Err (a String) that
-- describes the error.            
getErrorMessage :: FilePath -> IOError -> Err
getErrorMessage s e | isDoesNotExistError e  = s ++ " does not exist!"
                    | isAlreadyInUseError e  = s ++ " is already in use!"
                    | isPermissionError e    = "You do not have the permissions to access " ++ s
                    | otherwise              = "Operation generated an exception!"


-- | Function to handle the opening of a file, with any potential                    
catchReadFile :: FilePath -> IO FileContents
catchReadFile file = catch (readFile file) (handler file)
    where
        handler s = \e -> do
            putStrLn (getErrorMessage s e)
            cd <- getCurrentDirectory
                -- | Get string representing the current directory
            putStrLn ("Current directory is " ++ cd ++ "\nPlease write the name of the file you want: ")
                -- | Print out current directory, and request file from user.
                -- | Print out the current directory to make sure that the user
                -- is running the program in the right place!
            fileName <- getLine
            catchReadFile fileName
