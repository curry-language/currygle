{-# OPTIONS_FRONTEND -Wno-incomplete-patterns -Wno-unused-bindings #-}

module Index.Signature
    where

import Data.List  ( intercalate )
import System.IO

import FlatCurry.Types
import RW.Base

-- There are three types of Sigatures:
-- The Type, for example [Int] or IO a
-- Functions a -> Int
-- and variables, a or b
data Signature = Type String [Signature]
               | Function Signature Signature
               | Var Int
    deriving (Read, Show)

instance Eq Signature where
    (Var _) == (Type _ _) = False
    (Var _) == (Function _ _) = False
    (Type _ _) == (Var _) = False
    (Type _ _) == (Function _ _) = False
    (Function _ _) == (Var _) = False
    (Function _ _) == (Type _ _) = False
    (Var x) == (Var y) = x == y
    (Type n1 sigs1) == (Type n2 sigs2) = n1 == n2 && eqSigList sigs1 sigs2
    (Function s1 s2) == (Function s3 s4) = s1 == s3 && s2 == s4

-- Computes if the first signature list is equal to the second one, by testing elements,
-- until one is different
eqSigList :: [Signature] -> [Signature] -> Bool
eqSigList [] []                 = True
eqSigList [] (_:_)              = False
eqSigList (_:_) []              = False
eqSigList (s1:sigs1) (s2:sigs2) = if s1 == s2 then eqSigList sigs1 sigs2 else False

instance Ord Signature where
    (Var _) <= (Type _ _) = True
    (Var _) <= (Function _ _) = True
    (Type _ _) <= (Function _ _) = True
    (Type _ _) <= (Var _) = False
    (Function _ _) <= (Var _) = False
    (Function _ _) <= (Type _ _) = False
    (Var x) <= (Var y) = x <= y
    (Function x y) <= (Function v w) = if x == v then y <= w else x <= v
    (Type n1 sigs1) <= (Type n2 sigs2) = if n1 == n2 then leSigList sigs1 sigs2 else n1 <= n2

-- Computes if the first signature list is less or equal to the second one, by testing elements,
-- until one is different
leSigList :: [Signature] -> [Signature] -> Bool
leSigList [] []                 = True
leSigList [] (_:_)              = True
leSigList (_:_) []              = False
leSigList (s1:sigs1) (s2:sigs2) = if s1 == s2 then leSigList sigs1 sigs2 else s1 <= s2

typeExprToSignature :: TypeExpr -> Signature
typeExprToSignature (TVar i)            = Var i
typeExprToSignature (FuncType s1 s2)    = Function (typeExprToSignature s1) (typeExprToSignature s2)
-- We ignore the package name for Constant variable names in our search
typeExprToSignature (TCons (_,n2) tes) = Type n2 (map typeExprToSignature tes)
typeExprToSignature (ForallType _ te)   = typeExprToSignature te

-- Does a flat seperation of a signature to a list. So a -> b becomes [a,b],
-- and (a->b)->b becomes [(a->b), b]
seperateSig :: Signature -> [Signature]
seperateSig (Var x)             = [Var x]
seperateSig (Type x y)          = [Type x y]
seperateSig (Function s1 s2)    = s1 : seperateSig s2

-- Shows a list of Signatures as a Function
prettySigs :: [Signature] -> String
prettySigs [] = ""
prettySigs [x] = prettySig x
prettySigs (x:y:xs) = prettySig x ++ " -> " ++ prettySigs (y:xs)

-- Shows a single signature in a pretty way
prettySig :: Signature -> String
prettySig (Var i) = [chr (i + 97)]
prettySig (Function s1 s2) = "(" ++ prettySigs [s1, s2] ++ ")"
prettySig (Type name args)
  | name == "[]" && length args == 1
  = "[" ++ prettySig (head args) ++ "]"
  | take 2 name == "(,"
  = "(" ++ intercalate "," (map prettySig args) ++ ")"
  | otherwise
  = name ++ concatMap (\x -> " " ++ prettySig x) args

-- Auto generated code for rw-data

instance ReadWrite Signature where
  readRW strs ('0' : r0) = (Type a' b',r2)
    where
      (a',r1) = readRW strs r0
      (b',r2) = readRW strs r1
  readRW strs ('1' : r0) = (Function a' b',r2)
    where
      (a',r1) = readRW strs r0
      (b',r2) = readRW strs r1
  readRW strs ('2' : r0) = (Var a',r1)
    where
      (a',r1) = readRW strs r0

  showRW params strs0 (Type a' b') = (strs2,showChar '0' . (show1 . show2))
    where
      (strs1,show1) = showRW params strs0 a'
      (strs2,show2) = showRW params strs1 b'
  showRW params strs0 (Function a' b') =
    (strs2,showChar '1' . (show1 . show2))
    where
      (strs1,show1) = showRW params strs0 a'
      (strs2,show2) = showRW params strs1 b'
  showRW params strs0 (Var a') = (strs1,showChar '2' . show1)
    where
      (strs1,show1) = showRW params strs0 a'

  writeRW params h (Type a' b') strs =
    hPutChar h '0' >> (writeRW params h a' strs >>= writeRW params h b')
  writeRW params h (Function a' b') strs =
    hPutChar h '1' >> (writeRW params h a' strs >>= writeRW params h b')
  writeRW params h (Var a') strs = hPutChar h '2' >> writeRW params h a' strs

  typeOf _ = monoRWType "Signature"
