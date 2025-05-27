module Search.SignatureParser ( parseSignature )
 where

import Data.List

import FlatCurry.Types hiding (Var, Type)

import Index.Signature

-- Reads a String as a signature
-- It could start with ( in case of unit type, tuple or for function
-- Otherwise a List which starts with [ or a letter
parseSignature :: String -> Maybe Signature
parseSignature text = case splitFunctionArguments text of
  Nothing   -> Nothing
  Just args ->
    -- If it is a function, pass all sub-signatures, and make a function out of it
    if length args > 1
      then fromList (map parseSignature args)
      -- Otherwise differentiate according to the start of the signature
      else if head (trimf (head args)) == '('
             then parseParenth (trimf (head args))
             else if head (trimf (head args)) == '['
                    then parseList (trimf (head args))
                    else parseTypeOrVar (trimf (head args))
 where
  -- Parses a String into a type or variable, including lists and tupels
  parseTypeOrVar :: String -> Maybe Signature
  parseTypeOrVar [] = Nothing
  parseTypeOrVar (c:cs)
    | not (null(trimf (c:cs))) && isLower (head (trimf (c:cs)))
        = parseVar (c:cs)
    | not (null(trimf (c:cs))) && isUpper (head (trimf (c:cs)))
        = parseType (c:cs)
    | not (null(trimf (c:cs))) && head (trimf (c:cs)) == '['
        = parseList (c:cs)
    | not (null(trimf (c:cs))) && head (trimf (c:cs)) == '('
        = parseParenth (c:cs)
    | otherwise = Nothing
  -- Gets a Signature, and splits it by the arguments of the function,
  splitFunctionArguments :: String -> Maybe [String]
  splitFunctionArguments []     = Nothing
  splitFunctionArguments (c:cs) = case getNextFunArg (c:cs) [] 0 0 of
    Nothing           -> Nothing
    Just (next, left) ->
      if length left == 0
        then Just [next]
        else case splitFunctionArguments left of
               Nothing   -> Nothing
               Just args -> Just (next:args)
  getNextFunArg :: String -> String -> Int -> Int -> Maybe (String, String)
  getNextFunArg [] acc p b    | p == 0 && b == 0 = Just (acc, "")
                              | otherwise        = Nothing
  getNextFunArg (c:cs) acc p b
    | p == 0 && b == 0 && c == '-' && length cs > 0 && head cs == '>'
                = Just (acc, drop 1 cs)
    | c == '-' && (length cs < 1 || head cs /= '>') = Nothing
    | c == '('  = getNextFunArg cs (acc++"(") (p+1) b
    | c == ')'  = getNextFunArg cs (acc++")") (p-1) b
    | c == '['  = getNextFunArg cs (acc++"[") p (b+1)
    | c == ']'  = getNextFunArg cs (acc++"]") p (b-1)
    | otherwise = getNextFunArg cs (acc++[c]) p b

-- Parses a String into a List Signature for example [Int] or []
parseList :: String -> Maybe Signature
parseList str
  | null str || head (trimf str) /= '[' ||
    (removeSpaces str) !! (length (removeSpaces str) - 1) /= ']'
  = Nothing
  | otherwise
  = case parseSignature (removeLastBracket (drop 1 (trimf str)) 1) of
      Nothing   -> Nothing
      Just sig1 -> Just (Type "[]" [sig1])
 where
  removeLastBracket :: String -> Int -> String
  removeLastBracket []     _ = []
  removeLastBracket (c:cs) x | x==1 && c==']' = []
                             | x>1  && c==']' = c : removeLastBracket cs (x-1)
                             | c == '['       = c : removeLastBracket cs (x+1)
                             | otherwise      = c : removeLastBracket cs x

parseParenth :: String -> Maybe Signature
parseParenth str
  | take 2 str == "()" = Just (Type "()" [])
  | otherwise
  = let tupleSize = getTupleSize str
    in if tupleSize > 1
         then parseTuple tupleSize str
         else case removeLastParenth (drop 1 str) [] 1 of
                Nothing   -> Nothing
                Just str1 -> parseSignature str1
    --where
-- Deletes all elements of a string after all parenthesis are closed
removeLastParenth :: String -> String -> Int -> Maybe String
removeLastParenth []     _   _ = Nothing
removeLastParenth (c:cs) acc x
  | c == '('              = removeLastParenth cs (acc++[c]) (x+1)
  | c == ')' && x == 1    = Just acc
  | c == ')' && x > 1     = removeLastParenth cs (acc++[c]) (x-1)
  | otherwise             = removeLastParenth cs (acc++[c]) (x)

-- Gets the size of the tuple to be parsed, and the string containing a tuple
-- and returns the tuple as a signature
parseTuple :: Int -> String -> Maybe Signature
parseTuple n str =
   case parseTupleElements n (drop 1 str) of
     Nothing        ->  Nothing
     Just (sigs, _) -> if length sigs == 0
                         then Nothing
                         else Just
                              (Type ('(' : (take (n-1) (repeat ','))++")") sigs)
 where
  -- Parses x elements of a tuple from a string. The first parenthesis of the
  -- string should be removed before using this function
  parseTupleElements :: Int -> String -> Maybe ([Signature], String)
  parseTupleElements x input
    | x == 1 = case getNextElement input of
                 Nothing           -> Nothing
                 Just (next, left) -> Just ([next], left)
    | x >  1 = case getNextElement input of
                  Nothing           -> Nothing
                  Just (next, left) ->
                    case parseTupleElements (x-1) left of
                      Nothing            -> Nothing
                      Just (sigs, left2) -> Just (sigs ++ [next], left2)

  -- Gets a String containing the next Signature of the tuple
  getNextElement :: String -> Maybe (Signature, String)
  getNextElement input = getNextElementRec input [] 0

  getNextElementRec :: String -> String -> Int -> Maybe (Signature, String)
  getNextElementRec [] _ _        = Nothing
  getNextElementRec (x:xs) acc p
    | p == 0 && (x == ',' || x == ')')
    = case parseSignature acc of
          Nothing  -> Nothing
          Just sig -> Just (sig, xs)
    | x == '('           = getNextElementRec xs (acc ++ [x]) (p+1)
    | x == ')' && p > 0  = getNextElementRec xs (acc ++ [x]) (p-1)
    | otherwise          = getNextElementRec xs (acc ++ [x]) p

-- Checks how many , are exist before the first parenthesis is closed again
-- The first parenthesis should be removed beforehand
getTupleSize :: String -> Int
getTupleSize str = 1 + getTupleSizeHelp 0 str
 where
  getTupleSizeHelp :: Int -> String -> Int
  getTupleSizeHelp _ []       = 0
  getTupleSizeHelp p (x:xs)   | p == 1 && x == ',' = 1 + getTupleSizeHelp 0 xs
                              | x == '('           = getTupleSizeHelp (p+1) xs
                              | p == 1 && x == ')' = 0
                              | p >  1 && x == ')' = getTupleSizeHelp (p-1) xs
                              | otherwise          = getTupleSizeHelp p xs

-- Parses a Type from into a Signature. A type either begins
parseType :: String -> Maybe Signature
parseType str =
  let (next, left) = getNextElement str [] 0 0
  in if null next
       then Nothing
       else case signatruesOfItemForType left of
              Nothing ->  Nothing
              Just sigs -> if Type next sigs == Type "String" []
                             then Just (Type "[]" [Type "Char" []])
                             else Just (Type next sigs)
 where
  -- Gets the next Element of a type declaration.
  -- 1. The String to be analysed
  -- 2. The accumulator
  -- 3. Counter for parenthesis
  -- 4. Counter for Brackets
  getNextElement :: String -> String -> Int -> Int -> (String, String)
  getNextElement []     acc _ _ = (acc,[])
  -- Ignore all spaces infront of the next element
  getNextElement (c:cs) acc p b
    | null acc && c == ' ' = getNextElement cs acc p b
    -- A space finishes an element if all parenthesis
    -- and brackets are closed
    | not (null acc) && p == 0 && b == 0 && c == ' ' =
        (acc, cs)
    | c == '('  = getNextElement cs (acc++[c]) (p+1) b
    | c == ')'  = getNextElement cs (acc++[c]) (p-1) b
    | c == '['  = getNextElement cs (acc++[c]) p (b+1)
    | c == ']'  = getNextElement cs (acc++[c]) p (b-1)
    | otherwise = getNextElement cs (acc++[c]) p b

  signatruesOfItemForType :: String -> Maybe [Signature]
  signatruesOfItemForType []     = Just []
  signatruesOfItemForType (c:cs) =
    if null (trimf (c:cs))
      then Just []
      else let (next, left) = getNextElement (c:cs) [] 0 0
           in if null (trimf (c:cs))
                then Just []
                else case parseSignature next of
                       Nothing  -> Nothing
                       Just sig -> case signatruesOfItemForType left of
                                     Nothing   -> Nothing
                                     Just sigs -> Just (sigs ++ [sig])
                                    
-- Parses a single lowercase letter into a Var type Signature.
parseVar :: String -> Maybe Signature
parseVar []     = Nothing
parseVar (c:cs) =
  if null (trimf (c:cs))
    then Nothing
    else -- If there is only a single lowercase letter, and spaces before
         -- or after, take that letters encoding
       if isLower (head (trimf (c:cs))) && null (trimf (drop 1 (trimf (c:cs))))
         then Just (Var (ord (head (trimf (c:cs))) - 97))
         else Nothing

fromList :: [Maybe Signature] -> Maybe Signature
fromList []       = Nothing
fromList [Nothing] = Nothing
fromList [Just s]  = Just s
fromList (x:y:xs) = case x of Nothing -> Nothing
                              Just x1 -> let Just fun = fromList (y:xs)
                                         in Just (Function x1 fun)

-- Removes all spaces from the front
trimf :: String -> String
trimf str = dropWhile (\x -> x == ' ') str

-- Removes all spaces from a String
removeSpaces :: String -> String
removeSpaces = filter (/=' ')
