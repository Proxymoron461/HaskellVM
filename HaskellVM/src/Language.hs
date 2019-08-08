--------------------------------------------------------------------------------
-- | Defines the language in use.
module Language where

--------------------------------------------------------------------------------
-- | Importing things goes here

--------------------------------------------------------------------------------

data Program = ZipList (Int, Stmt)

data Stmt
    = JumpTo { 
        lineNo :: Int
    }
    | JumpToWhenZero {
        lineNo :: Int,
        location :: Var
    }
    | SetValueToVar {
        destvar :: Var,
        val :: Int
    }
    | SetVarToVar {
        destvar :: Var,
        supplyvar :: Var
    }
    | AddValueToVar {
        destvar :: Var,
        val :: Int
    }
    | AddVarToVar {
        destvar :: Var,
        supplyvar :: Var
    }
    | End

data Var = Address Int | Name String | Value Int

type Memory = [(Int, Int)]
type Err = String