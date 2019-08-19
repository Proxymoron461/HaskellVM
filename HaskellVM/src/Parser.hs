--------------------------------------------------------------------------------
-- | Takes error-free string input, and hands a program to the Interpreter.
module Parser where

import Language
import Zipper
import Data.Either
import Data.Functor
import Control.Applicative
import qualified Data.ByteString as BS

--------------------------------------------------------------------------------

-- | Definition of my own parser type
data Parser a = Parser (String -> Either Err (a, String))

-- | Functor instance for Parser
instance Functor Parser where
    -- | fmap :: (a -> b) -> f a -> f b
    fmap f (Parser g) = Parser $ \x -> first f <$> g x

-- | Applicative instance for Parser
instance Applicative Parser where
    -- | pure :: a -> f a
    pure x = Parser $ \xs -> Right (x, xs)
    
    -- | (<*>) :: f (a -> b) -> f a -> f b
    -- | Need to apply g to x, and then apply f to (g x), but ONLY if both are
    -- successful.
    -- | g :: String -> Either Err (a, String)
    -- | f :: String -> Either Err (a -> b, String)
    -- Parser f <*> Parser g = Parser $ \x -> case g x of
    --     Left s -> Left s
    --     Right (y, ys) -> case f ys of
    --         Left s -> Left s
    --         Right (z, zs) -> Right (z y, zs)
    Parser f <*> Parser g = Parser $ \x ->
        g x >>= (\(y,ys) -> first ($ y) <$> f ys)
        

-- | Alternative instance for Parser
instance Alternative Parser where
    -- | empty :: f a
    empty = undefined

    -- | (<|>) :: f a -> f a -> f a
    (<|>) = undefined

    -- | some :: f a -> f [a]
    some = undefined

    -- | many :: f a -> f [a]
    many = undefined

-- | Monad instance for Parser
instance Monad Parser where
    -- | return :: a -> m a
    return = pure

    -- | (>>=) :: m a -> (a -> m b) -> m b
    -- Parser f >>= g = Parser $ \x -> case f x of
    --     Left s -> Left s
    --     Right (y,ys) -> parse ys (g y)
    Parser f >>= g = Parser $ \x -> either Left (\(y,ys) -> parse ys (g y)) (f x)

parse :: String -> Parser a -> Either Err (a, String)
parse x (Parser f) = f x

-- | Applies a function to the first element of a tuple.
first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

--------------------------------------------------------------------------------


-- | Program :: Zipper (LineNo, Instruction)
parseFile :: FileContents -> Either Err Program
parseFile fileString = Left "parseFile not defined yet"
    where
        file = formatFile fileString
            -- | Split the file contents into a list, where each list element is
            -- a line. Remove all blank lines, and assign a line number to each
            -- element.

-- | Efficient version of parseFile which will read the file in a strict manner.        
parseFile' :: BS.ByteString -> Either Err Program
parseFile' = undefined

removeBlankLines :: [FileContents] -> [FileContents]
removeBlankLines = filter (/= "")

numberLines :: [FileContents] -> [(Int, FileContents)]
numberLines = zip [0..]

formatFile :: FileContents -> [(Int, FileContents)]
formatFile = numberLines . removeBlankLines . lines