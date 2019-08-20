--------------------------------------------------------------------------------
-- | Takes error-free string input, and hands a program to the Interpreter.
module Parser where

import Language
import Zipper
import Data.Either
import Data.Functor
import Data.Char
import Control.Applicative
import Control.Monad
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
    -- | Below, (>>=) :: Either Err (a, String) ->
    --       (a -> Either Err (b, String)) -> Either Err (b, String)
    -- Parser f <*> Parser g = Parser $ \x ->
    --     g x >>= \(y,ys) -> first ($ y) <$> f ys
    Parser f <*> Parser g = Parser $ g >=> h
        where h (y,ys) = first ($ y) <$> f ys
        

-- | Alternative instance for Parser
instance Alternative Parser where
    -- | empty :: f a
    empty = Parser . const . Left $ "Empty Computation"

    -- | (<|>) :: f a -> f a -> f a
    -- Parser f <|> Parser g = Parser $ \x -> case f x of
    --     Right (y,ys) -> Right (y,ys)
    --     Left s -> g x
    Parser f <|> Parser g = Parser $ \x -> either (const $ g x) Right (f x)

    -- | some :: f a -> f [a]
    -- | Return Left Err if all computations fail - otherwise, return a list
    -- of successful computations.
    -- | some ~ (String -> Either Err (a, String)) -> (String -> Either Err ([a], String))
    -- | f :: String -> Either Err (a, String)
    -- | s :: String
    some (Parser f) = Parser $ \s -> case f s of
        Left e        -> Left e
        Right (x, xs) -> first (x :) <$> mFoo f xs
        --where
            -- foo s = case f s of
            --     Left e        -> Left e
            --     Right (x, xs) -> first (x :) <$> foo xs
            --
            -- foo s = f s >>= \(x,xs) -> first (x :) <$> foo xs
                -- | Above solutions will make whole thing fail if one fails and all
                -- others succeed. Not good enough.
    --some (Parser f) = Parser f <|> many (Parser f)
        -- | Above solution does not work - many (Parser f) has type Parser [a], 
        -- not type Parser a. But I feel I am onto something.

    -- | many :: f a -> f [a]
    -- | Can return an empty list if all computations fail - otherwise, return
    -- a list of successful computations.
    -- | many :: Parser a -> Parser [a]
    -- | many ~ (String -> Either Err (a, String)) -> (String -> Either Err ([a], String))
    -- | f :: String -> Either Err (a, String)
    -- | foo :: String -> Either Err ([a], String)
    -- | foo xs :: Either Err ([a], String)
    -- many (Parser f) = Parser foo
    --     where
    --         foo s = case f s of
    --             Left e        -> Right ([], s)
    --             Right (x, xs) -> first (x :) <$> foo xs
    many (Parser f) = Parser $ mFoo f
        -- | Above solution uses helper function, mFoo. It is not let- or where-bound
        -- because it is also helpful in defining some.

-- | Helper function for many - not let- or where-bound because it is also used
-- for some.
mFoo f s = either (const $ Right ([], s)) (\(x,xs) -> first (x :) <$> mFoo f xs) (f s)

-- | Monad instance for Parser
instance Monad Parser where
    -- | return :: a -> m a
    return = pure

    -- | (>>=) :: m a -> (a -> m b) -> m b
    -- Parser f >>= g = Parser $ \x -> case f x of
    --     Left s -> Left s
    --     Right (y,ys) -> parse ys (g y)
    -- | f :: String -> Either Err (a, String)
    -- | g :: a -> Parser b
    -- Parser f >>= g = Parser $ \x -> either Left (\(y,ys) -> parse ys (g y)) (f x)
    -- Parser f >>= g = Parser $ \x -> f x >>= (\(y,ys) -> parse ys (g y))
    -- | h :: (a, String) -> Either Err (b, String)
    -- | (>=>) :: (a -> m b) -> (b -> m c) -> a -> m c
    Parser f >>= g = Parser $ f >=> h
        where h (y,ys) = parse (g y) ys

parse :: Parser a -> String -> Either Err (a, String)
parse (Parser f) = f

-- | Applies a function to the first element of a tuple.
first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

--------------------------------------------------------------------------------
-- | Definitions of a few useful parsers

-- | Parse a character based on a predicate
parseIf :: (Char -> Bool) -> Parser Char
parseIf p = Parser foo
    where
        foo ""                 = Left "Parse failed - empty string"
        foo (x:xs) | p x       = Right (x,xs)
                   | otherwise = Left $ "Parse failed with character: " ++ show x

-- | Parse a character if it is a member of a given list
oneOf :: [Char] -> Parser Char
oneOf cs = parseIf (`elem` cs)

-- | Parse whitespace
whitespace :: Parser Char
whitespace = parseIf isSpace

-- | Parse and remove whitespace
trim :: Parser String
trim = undefined

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

-- | Function that takes the contents of a file as input, and formats it into a
-- list of individual lines, each indexed by line number.
formatFile :: FileContents -> [(Int, FileContents)]
formatFile = numberLines . removeBlankLines . lines