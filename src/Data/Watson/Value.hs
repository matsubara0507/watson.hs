module Data.Watson.Value where

import           Data.ByteString     (ByteString)
import           Data.HashMap.Strict (HashMap)
import           Data.Int            (Int64)
import           Data.Vector         (Vector)
import           Data.Word           (Word64)

data Value
  = Int !Int64
  | UInt !Word64
  | Float !Double
  | String !ByteString
  | Object !(HashMap ByteString Value)
  | Array !(Vector Value)
  | Bool !Bool
  | Nil
  deriving (Show, Eq)

zero :: Value
zero = Int 0

inf, nan :: Value
inf = Float (1/0)
nan = Float (0/0)

emptyS, emptyO, emptyA :: Value
emptyS = String mempty
emptyO = Object mempty
emptyA = Array mempty

data Type
  = IntT
  | UIntT
  | FloatT
  | StringT
  | ObjectT
  | ArrayT
  | BoolT
  | NilT
  deriving (Show, Eq)

valueToType :: Value -> Type
valueToType (Int _)    = IntT
valueToType (UInt _)   = UIntT
valueToType (Float _)  = FloatT
valueToType (String _) = StringT
valueToType (Object _) = ObjectT
valueToType (Array _)  = ArrayT
valueToType (Bool _)   = BoolT
valueToType Nil        = NilT
