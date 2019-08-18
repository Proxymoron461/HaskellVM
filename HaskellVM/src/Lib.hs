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
import Control.Monad (when)


entry :: IO ()
entry = input


-- | Take in user's command line input for the file name, bind it to a variable,
-- and hand that to the parser.
input :: IO ()
input = do
            args <- getArgs
                -- | Get [String] of command line arguments.
            fileContents <-
                if (length args /= 0) then
                    let fileName = head args in catchReadFile fileName
                else requestFileNameFromUser
                    -- | Request that the user enter the name of the file that
                    -- they want manually, instead of providing it as a command
                    -- line argument.
            either putStrLn putStrLn (parse fileContents >>= interpret >>= output)
                -- | Whether output is memory or error, print to screen!



-- | Given a FilePath (a String) and an IOError, returns an Err (a String) that
-- describes the error.
-- | Uses predicates found in System.IO.Error in order to determine the precise
-- nature of the exception. If it is not one of the usual suspects, then a 
-- standard error message is returned.
getErrorMessage :: FilePath -> IOError -> Err
getErrorMessage s e | isDoesNotExistError e  = s ++ " does not exist!"
                    | isAlreadyInUseError e  = s ++ " is already in use!"
                    | isPermissionError e    = "You do not have the permissions to access " ++ s
                    | otherwise              = "Attempting to read " ++ s ++ " generated an exception!"


-- | Function to handle the opening of a file, with any potential IOErrors caught.                   
catchReadFile :: FilePath -> IO FileContents
catchReadFile fileName = catch (readFile fileName) (handler fileName)
    where
        handler file = \err -> do
            putStrLn (getErrorMessage file err)
            requestFileNameFromUser


requestFileNameFromUser :: IO FileContents
requestFileNameFromUser = do
            cd <- getCurrentDirectory
                -- | Get string representing the current directory
            putStrLn ("Current directory is " ++ cd ++ "\nPlease write the name of the file you want: ")
                -- | Print out current directory, and request file from user.
                -- | Print out the current directory to make sure that the user
                -- is running the program in the right place!
            fileName <- getLine
            catchReadFile fileName
