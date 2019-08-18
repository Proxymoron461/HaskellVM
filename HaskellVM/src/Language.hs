--------------------------------------------------------------------------------
-- | Defines the language in use.
module Language where

--------------------------------------------------------------------------------
-- | Importing things goes here
import Zipper
--------------------------------------------------------------------------------

-- | Data type for language instructions (aka mnemonics)
data Instruction
    = JumpTo { 
        lineNo :: LineNo
    }
    | JumpToWhenZero {
        lineNo :: LineNo,
        location :: Operand
    }
    | SetValueToVar {
        destvar :: Operand,
        val :: Int
    }
    | SetVarToVar {
        destvar :: Operand,
        supplyvar :: Operand
    }
    | AddValueToVar {
        destvar :: Operand,
        val :: Int
    }
    | AddVarToVar {
        destvar :: Operand,
        supplyvar :: Operand
    }
    | End


-- | Data type to represent operands to instructions
data Operand = Address Address | Name String | Value Int

-- | Address is type synonym for Int
type Address = Int

-- | Memory is type synonym for association list of address and value
type Memory = [(Address, Int)]

-- | Error is type synonym for a String, describing the problem
type Err = String

-- | LineNo is type synonym for Int
type LineNo = Int

-- | FileContents is type synonym for String
type FileContents = String

-- | Program is type synonym for Zipper
type Program = Zipper (LineNo, Instruction)