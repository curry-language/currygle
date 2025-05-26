{-# OPTIONS_FRONTEND -Wno-incomplete-patterns #-}

module Index.Signature
    where

import Data.List  ( intercalate )
import System.IO

import FlatCurry.Types
import RW.Base

-- There are three types of Sigatures:
-- a `Type`, i.e., a type constructor applied to arguments,
-- like `[Int]` or `IO a`,
-- a `Function`, e.g., `[a] -> Int`,
-- or a variable with some index.
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

--- Normalize a `Signature` expression by enumerating all type variable
--- from 0 in the order of their occurrences.
--- Hence, `map` has always type `(a -> b) -> [a] -> [b]` instead of the type
--- `(b -> a) -> [b] -> [a]` obtained from CurryDoc.
normalizeSignature :: Signature -> Signature
normalizeSignature sig = mapVars (zip (reverse (varsInSig [] sig)) [0..]) sig
 where
  varsInSig vs (Var i)          = if i `elem` vs then vs else i:vs
  varsInSig vs (Function t1 t2) = varsInSig (varsInSig vs t1) t2
  varsInSig vs (Type _ ts)      = foldl varsInSig vs ts

  mapVars mvs (Var i)          =
    Var (maybe (error "Var not found in normalizeSignature") id (lookup i mvs))
  mapVars mvs (Function t1 t2) = Function (mapVars mvs t1) (mapVars mvs t2)
  mapVars mvs (Type tc ts)     = Type tc (map (mapVars mvs) ts)

typeExprToSignature :: TypeExpr -> Signature
typeExprToSignature = normalizeSignature . typeExp2Sig
 where
  typeExp2Sig (TVar i)           = Var i
  typeExp2Sig (FuncType s1 s2)   = Function (typeExp2Sig s1) (typeExp2Sig s2)
  -- ignore module name since we do not support qualified names in our search
  typeExp2Sig (TCons (_,n2) tes) = Type n2 (map typeExp2Sig tes)
  typeExp2Sig (ForallType _ te)  = typeExp2Sig te

signatureToTypeExpr :: Signature -> TypeExpr
signatureToTypeExpr (Var i)          = (TVar i)
signatureToTypeExpr (Function s1 s2) =
  FuncType (signatureToTypeExpr s1) (signatureToTypeExpr s2)
signatureToTypeExpr (Type n2 tes)    =
  TCons ("",n2) (map signatureToTypeExpr tes)

-- Does a flat seperation of a signature to a list. So a -> b becomes [a,b],
-- and (a->b)->b becomes [(a->b), b]
seperateSig :: Signature -> [Signature]
seperateSig (Var x)             = [Var x]
seperateSig (Type x y)          = [Type x y]
seperateSig (Function s1 s2)    = s1 : seperateSig s2

-- Shows a list of Signatures as a Function
prettySigs :: [Signature] -> String
prettySigs []       = ""
prettySigs [x]      = prettySig False x
prettySigs (x:y:xs) = case isClassContext x of
  Nothing      -> prettySig False (foldr1 Function (x:y:xs))
  Just (tc,ts) -> prettySig False (Type tc ts) ++ " => " ++ prettySigs (y:xs)

-- Shows a single signature in a pretty way
prettySig :: Bool -> Signature -> String
prettySig _  (Var i)          = [chr (i + 97)]
prettySig br (Function s1 s2) = bracketIf br (prettySig True s1 ++ " -> " ++
                                              prettySig False s2)
prettySig br (Type name args)
  | name == "[]" && length args == 1
  = let arg = head args
    in if arg == Type "Char" []
          then "String"
          else "[" ++ prettySig False arg ++ "]"
  | take 2 name == "(,"
  = "(" ++ intercalate "," (map (prettySig False) args) ++ ")"
  | name == "Apply" && length args == 2 -- type constructore application
  = bracketIf br (prettySig True (head args) ++ " " ++ prettySig True (args!!1))
  | otherwise
  = bracketIf (br && not (null args))
      (intercalate " " (name : map (prettySig True) args))

bracketIf :: Bool -> String -> String
bracketIf wb s = if wb then "(" ++ s ++ ")" else s

--- Tests whether a FlatCurry type is a class context.
--- If it is the case, return the class name and the type parameters
--- of the class context.
isClassContext :: Signature -> Maybe (String,[Signature])
isClassContext texp = case texp of
  Type tc ts                           -> checkDictCons tc ts
  -- a class context might be represented as function `() -> Dict`:
  Function (Type "()" []) (Type tc ts) -> checkDictCons tc ts
  _                                    -> Nothing
 where
  checkDictCons tc ts | take 6 tc == "_Dict#" = Just (drop 6 tc, ts)
                      | otherwise             = Nothing

------------------------------------------------------------------------------
-- Auto generated by curry-rw-data
------------------------------------------------------------------------------

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
  
------------------------------------------------------------------------------
