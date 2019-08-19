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
import qualified Data.ByteString as BS
import Data.List (isSuffixOf)
import Data.Maybe (isJust, fromJust)


entry :: IO ()
entry = input


-- | Take in user's command line input for the file name, bind it to a variable,
-- and hand that to the parser.
-- | TODO: Handle multiple command line arguments - what if
-- fileName isn't first?
input :: IO ()
input = do args <- getArgs
               -- | Get [String] of command line arguments.
           fileContents <- let maybeFileName = safeHead args in
               ucMultiIf (isJust maybeFileName) requestFileNameFromUser $
                   let fileName = fromJust maybeFileName in
                   (fileExtension `isSuffixOf` fileName, catchReadFile fileName)
               -- | Request that the user enter the name of the file that
               -- they want manually, instead of providing it as a command
               -- line argument.
           either putStrLn putStrLn (parse fileContents >>= interpret >>= output)
               -- | Whether output is memory or error, print to screen!
               -- | TODO: May need editing later.


-- | If the first predicate is true, then case1 is executed. If the second is
-- true, then case2 is executed.
-- | They are in the order given so that lazy evaluation can be used to define
-- a helpful success case, with any let-bound expressions bound to both the
-- second predicate and the success case.
-- | This function enables (if (A && B) then C else D) control flow statements
-- where a let-bound expression, say x, can be bound to both B and C, preventing
-- unnecessary repetition (such as calling head args twice). To successfully 
-- do this, however, the use of uncurry is recommended. (Though a second, 
-- similar function is below, with the uncurry behaviour built-in to support
-- this multiple-expression binding.)
-- | Because of the above, control flow statements such as:
-- (if (A) then (if (B) then C else D) else D)
-- are made unnecessary, even if you need a let-bound variable for both B and C
-- whose existence is dependent on A.
-- | For instance, if A checks whether a particular variable is Nothing or
-- Just x, then x can be let-bound for both B and C despite possibly not
-- existing, through the magic of lazy evaluation.
multiIf :: Bool -> a -> Bool -> a -> a
multiIf True _ True case1 = case1
multiIf False case2 _ _   = case2
multiIf _ case2 False _   = case2

-- | Partially uncurried version of multiIf, for simplifying the common use case
-- of:
-- uncurry (multiIf w x) $ (y, z)
ucMultiIf :: Bool -> a -> (Bool, a) -> a
ucMultiIf b1 c2 = uncurry $ multiIf b1 c2


-- | Safely fetch the head from a list               
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x               


-- | Given a FilePath (a String) and an IOError, returns an Err (a String) that
-- describes the error.
-- | Uses predicates found in System.IO.Error in order to determine the precise
-- nature of the exception. If it is not one of the usual suspects, then a 
-- standard error message is returned.
getErrorMessage :: FilePath -> IOError -> Err
getErrorMessage s e | isDoesNotExistError e  = s ++ " does not exist!"
                    | isAlreadyInUseError e  = s ++ " is already in use!"
                    | isPermissionError e    = "You do not have the permissions to access " ++ s
                    | not (fileExtension `isSuffixOf` s) = "File has the wrong type!"
                    | otherwise              = "Attempting to read " ++ s ++ " generated an exception!"


-- | Function to handle the opening of a file, with any potential IOErrors caught.                   
catchReadFile :: FilePath -> IO FileContents
catchReadFile fileName = catch (readFile fileName) (handler fileName)
    where
        handler file err = do
            putStrLn (getErrorMessage file err)
                -- | Print out error message
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
