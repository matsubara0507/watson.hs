{-# LANGUAGE TupleSections #-}

module Data.Watson where

import           Data.Bifunctor        (first)
import           Data.Bits             (unsafeShiftL)
import qualified Data.ByteString       as BS
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BSC
import           Data.Foldable         (foldlM)
import qualified Data.HashMap.Strict   as HM
import           Data.Int              (Int64)
import qualified Data.List             as List
import qualified Data.Vector           as V
import           Data.Watson.Value     as W
import           Unsafe.Coerce         (unsafeCoerce)

data Instructions
  = Inew
  | Iinc
  | Ishl
  | Iadd
  | Ineg
  | Isht
  | Itof
  | Itou
  | Finf
  | Fnan
  | Fneg
  | Snew
  | Sadd
  | Onew
  | Oadd
  | Anew
  | Aadd
  | Bnew
  | Bneg
  | Nnew
  | Gdup
  | Gpop
  | Gswp
  deriving (Show, Eq)

data Mode = A | S deriving (Show, Eq)

data VM = VM
  { stack :: [W.Value]
  , mode  :: Mode
  , point :: (Int, Int)
  } deriving (Show, Eq)

data Error
  = StackEmpty
  | TypeMismatch W.Type W.Value -- expected and acutual
  deriving (Show, Eq)

newVM :: VM
newVM = VM [] A (1, 1)

incPoint :: Char -> VM -> VM
incPoint '\n' vm = vm { point = (fst (point vm) + 1, 1) }
incPoint _ vm    = vm { point = (+ 1) <$> point vm }

changeMode :: VM -> VM
changeMode vm =
  case mode vm of
    A -> vm { mode = S }
    S -> vm { mode = A }

pop :: VM -> Either Error (W.Value, VM)
pop vm =
  maybe (Left StackEmpty) (pure . fmap (\s -> vm { stack = s })) $ List.uncons (stack vm)

push :: W.Value -> VM -> VM
push v vm =
  vm { stack = (v : stack vm) }

mapInt :: (Int64 -> W.Value) -> VM -> Either Error VM
mapInt f vm = do
  (v, vm') <- pop vm
  case v of
    W.Int n -> pure $ push (f n) vm'
    _       -> Left $ TypeMismatch W.IntT v

mapInt2 :: (Int64 -> Int64 -> W.Value) -> VM -> Either Error VM
mapInt2 f vm = do
  (v, vm') <- pop vm
  case v of
    W.Int n -> mapInt (f n) vm'
    _       -> Left $ TypeMismatch W.IntT v

mapFloat :: (Double -> W.Value) -> VM -> Either Error VM
mapFloat f vm = do
  (v, vm') <- pop vm
  case v of
    W.Float n -> pure $ push (f n) vm'
    _         -> Left $ TypeMismatch W.FloatT v

step :: Char -> VM -> Either Error VM
step c vm =
  fmap (incPoint c) $ maybe (pure vm) (flip interp vm) $ toInstructions (mode vm) c

interp :: Instructions -> VM -> Either Error VM
interp Inew vm = pure $ push W.zero vm
interp Iinc vm = mapInt (W.Int . (+ 1)) vm
interp Ishl vm = mapInt (W.Int . flip unsafeShiftL 1) vm
interp Iadd vm = mapInt2 (\x y -> W.Int $ x + y) vm
interp Ineg vm = mapInt (W.Int . negate) vm
interp Isht vm = mapInt2 (\x y -> W.Int $ unsafeShiftL x (fromIntegral y)) vm
interp Itof vm = mapInt (W.Float . unsafeCoerce) vm
interp Itou vm = mapInt (W.UInt . fromIntegral) vm
interp Finf vm = pure $ push W.inf vm
interp Fnan vm = pure $ push W.nan vm
interp Fneg vm = do
  (v, vm') <- pop vm
  case v of
    W.Float n -> pure $ push (W.Float $ negate n) vm'
    _         -> Left $ TypeMismatch W.FloatT v
interp Snew vm = pure $ push W.emptyS (changeMode vm)
interp Sadd vm = do
  (v1, vm1) <- pop vm
  (v2, vm2) <- pop vm1
  case (v1, v2) of
    (W.Int n, W.String s) -> pure $ push (W.String $ BS.snoc s $ fromIntegral n) vm2
    (W.Int n, _)          -> Left $ TypeMismatch W.StringT v2
    _                     -> Left $ TypeMismatch W.IntT v1
interp Onew vm = pure $ push W.emptyO vm
interp Oadd vm = do
  (v1, vm1) <- pop vm
  (v2, vm2) <- pop vm1
  (v3, vm3) <- pop vm2
  case (v2, v3) of
    (W.String k, W.Object o) -> pure $ push (W.Object $ HM.insert k v1 o) vm3
    (W.String k, _)          -> Left $ TypeMismatch W.ObjectT v3
    _                        -> Left $ TypeMismatch W.StringT v2
interp Anew vm = pure $ push W.emptyA vm
interp Aadd vm = do
  (v1, vm1) <- pop vm
  (v2, vm2) <- pop vm1
  case v2 of
    W.Array a -> pure $ push (W.Array $ V.snoc a v1 ) vm2
    _         -> Left $ TypeMismatch W.ArrayT v2
interp Bnew vm = pure $ push (W.Bool False) vm
interp Bneg vm = do
  (v, vm') <- pop vm
  case v of
    W.Bool b -> pure $ push (W.Bool $ not b) vm'
    _        -> Left $ TypeMismatch W.BoolT v
interp Nnew vm = pure $ push W.Nil vm
interp Gdup vm = flip push vm <$> fmap fst (pop vm)
interp Gpop vm = snd <$> pop vm
interp Gswp vm = do
  (v1, vm1) <- pop vm
  (v2, vm2) <- pop vm1
  pure $ push v2 (push v1 vm)

toInstructions :: Mode -> Char -> Maybe Instructions
toInstructions A 'B'  = Just Inew
toInstructions A 'u'  = Just Iinc
toInstructions A 'b'  = Just Ishl
toInstructions A 'a'  = Just Iadd
toInstructions A 'A'  = Just Ineg
toInstructions A 'e'  = Just Isht
toInstructions A 'i'  = Just Itof
toInstructions A '\'' = Just Itou
toInstructions A 'q'  = Just Finf
toInstructions A 't'  = Just Fnan
toInstructions A 'p'  = Just Fneg
toInstructions A '?'  = Just Snew
toInstructions A '!'  = Just Sadd
toInstructions A '~'  = Just Onew
toInstructions A 'M'  = Just Oadd
toInstructions A '@'  = Just Anew
toInstructions A 's'  = Just Aadd
toInstructions A 'z'  = Just Bnew
toInstructions A 'o'  = Just Bneg
toInstructions A '.'  = Just Nnew
toInstructions A 'E'  = Just Gdup
toInstructions A '#'  = Just Gpop
toInstructions A '%'  = Just Gswp
toInstructions S 'S'  = Just Inew
toInstructions S 'h'  = Just Iinc
toInstructions S 'a'  = Just Ishl
toInstructions S 'k'  = Just Iadd
toInstructions S 'r'  = Just Ineg
toInstructions S 'A'  = Just Isht
toInstructions S 'z'  = Just Itof
toInstructions S 'i'  = Just Itou
toInstructions S 'm'  = Just Finf
toInstructions S 'b'  = Just Fnan
toInstructions S 'u'  = Just Fneg
toInstructions S '$'  = Just Snew
toInstructions S '-'  = Just Sadd
toInstructions S '+'  = Just Onew
toInstructions S 'g'  = Just Oadd
toInstructions S 'v'  = Just Anew
toInstructions S '?'  = Just Aadd
toInstructions S '^'  = Just Bnew
toInstructions S '!'  = Just Bneg
toInstructions S 'y'  = Just Nnew
toInstructions S '/'  = Just Gdup
toInstructions S 'e'  = Just Gpop
toInstructions S ':'  = Just Gswp
toInstructions _ _    = Nothing

decode :: ByteString -> Either (Error, VM) W.Value
decode s = do
  vm <- decode' s
  case stack vm of
    (v : _) -> pure v
    _       -> Left (StackEmpty, vm)

decode' :: ByteString -> Either (Error, VM) VM
decode' = flip BSC.foldl' (pure newVM) $ \m c ->
  case m of
    Left _   -> m
    Right vm -> first (,vm) $ step c vm

decodeFile :: FilePath -> IO (Either (Error, VM) W.Value)
decodeFile path = decode <$> BSC.readFile path
