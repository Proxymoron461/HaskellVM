--------------------------------------------------------------------------------
-- | Defines the language in use.
module Language where

--------------------------------------------------------------------------------
-- | Importing things goes here
import Zipper
import qualified Data.ByteString as BS

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

-- | LineNo is type synonym for Int, giving the line number
type LineNo = Int

-- | FileContents is type synonym for String, giving the contents of a file
type FileContents = String
-- type FileContents = BS.ByteString

-- | Program is type synonym for Zipper, containing instructions indexed by line
-- number
type Program = Zipper (LineNo, Instruction)

-- | Holds a string for the file type (like .txt, .java, and so on).
-- | TODO: discuss with Dilan!
fileExtension :: String
fileExtension = ".as"