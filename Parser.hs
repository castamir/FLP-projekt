module Parser
( BTree(..)
, regexpToBTree
)where

import qualified Data.Set as Set

type Symbol = Set.Set Char

data BTree = Leaf Symbol | Branch BTree Char BTree 
  deriving (Eq,Show)

expandRange from to =
  if from < to
    then [from .. to]
    else [to .. from]

expandList str = f [] str
  where
    f p [] = p
    f p (x:'-':z:xs) =(expandRange x z) ++ (f p xs)
    f p (x:xs) = x : (f p xs)


allowedSymbolSet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 "
insideSetSymbols = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 -"

getSet [] = Set.empty
getSet ('^':xs) = Set.difference (Set.fromList allowedSymbolSet) (Set.fromList (expandList xs))                 
getSet (x:xs) = Set.fromList (expandList (x:xs))
  
getBracketBlockPrefix (x:xs) = f [] xs 1
  where
    f p [] d  | d == 1 = p
              | otherwise = error "REGEXP.parse: missing ')'."
    f p (x:xs) d  | x == '(' && d > 0  = x : (f p xs (d+1))
                  | x == ')' && d == 1 = p
                  | x == ')' && d > 1  = x : (f p xs (d-1))
                  | otherwise          = x : (f p xs d)

getBracketBlockSuffix (x:xs) = drop (2 + length (getBracketBlockPrefix (x:xs))) (x:xs)

getUnionBlockPrefix (x:xs) = f [] (x:xs) 0
  where
    f p [] d  | d == 0 = p
              | otherwise = error "REGEXP.parse: missing '('."
    f p (x:xs) d  | x == '('            = x : (f p xs (d+1))
                  | x == ')' && d < 1   = error "REGEXP.parse: unexpected ')'."
                  | x == ')'            = x : (f p xs (d-1))
                  | x == '|' && d == 0  = p
                  | otherwise           = x : (f p xs d)

getUnionBlockSuffix (x:xs) = drop (length (getUnionBlockPrefix (x:xs))) (x:xs)

concatTree []       = Leaf Set.empty
concatTree (']':xs) = error "REGEXP.parse: unexpected ']'."
concatTree (x:'*':xs)
  | x == '.'  = Branch (Branch (Leaf (Set.fromList allowedSymbolSet)) '*' (Leaf Set.empty)) '.' (concatTree xs)
  | otherwise = Branch (Branch (Leaf (Set.singleton x)) '*' (Leaf Set.empty)) '.' (concatTree xs)
concatTree (x:'+':xs)
  | x == '.'  = Branch (Branch (Leaf (Set.fromList allowedSymbolSet)) '.' (Branch (Leaf (Set.fromList allowedSymbolSet)) '*' (Leaf Set.empty))) '.' (concatTree xs)
  | otherwise = Branch (Branch (Leaf (Set.singleton x)) '.' (Branch (Leaf (Set.singleton x)) '*' (Leaf Set.empty))) '.' (concatTree xs)
concatTree (x:'?':xs)
  | x == '.'  = Branch (Branch (Leaf (Set.fromList allowedSymbolSet)) '+' (Leaf Set.empty)) '.' (concatTree xs)
  | otherwise = Branch (Branch (Leaf (Set.singleton x)) '+' (Leaf Set.empty)) '.' (concatTree xs)
concatTree (x:xs)
  | x == '.'  = Branch (Leaf (Set.fromList allowedSymbolSet)) '.' (concatTree xs)
  | x == '['  = f (takeWhile (`elem` insideSetSymbols) xs) (dropWhile (`elem` insideSetSymbols) xs)   
  | otherwise = Branch (Leaf (Set.singleton x)) '.' (concatTree xs)
    where
      f p (']':'*':xs)  = Branch (Branch (Leaf (getSet p)) '*' (Leaf Set.empty)) '.' (concatTree xs)
      f p (']':'+':xs)  = Branch (Branch (Leaf (getSet p)) '.' (Branch (Leaf (getSet p)) '*' (Leaf Set.empty))) '.' (concatTree xs)
      f p (']':'?':xs)  = Branch (Branch (Leaf (getSet p)) '+' (Leaf Set.empty)) '.' (concatTree xs)
      f p (']':xs)      = Branch (Leaf (getSet p)) '.' (concatTree xs)
      f _ []            = error "REGEXP.parse: missing ']'."  
      f _ (x:xs)        = error ("REGEXP.parse: unexpected symbol " ++ [x] ++ ".")  

bracketTree [] = Leaf Set.empty
bracketTree s@('(':xs) = f (getBracketBlockPrefix s) (getBracketBlockSuffix s)
  where
    f p ('*':xs) = Branch (Branch (unionTree p) '*' (Leaf Set.empty)) '.' (unionTree xs)
    f p ('+':xs) = Branch (Branch (unionTree p) '.' (Branch (unionTree p) '*' (Leaf Set.empty))) '.' (unionTree xs)
    f p ('?':xs) = Branch (Branch (unionTree p) '+' (Leaf Set.empty)) '.' (unionTree xs)
    f p s = Branch (unionTree p) '.' (unionTree s)
bracketTree (x:xs) = f (takeWhile ('('/=) (x:xs)) (x:xs)
  where
    f p o =
      if p == o
        then concatTree p
        else Branch (concatTree p) '.' (bracketTree (drop (length p) o))

unionTree [] = Leaf Set.empty
unionTree s@(x:xs) = f (getUnionBlockPrefix s) (getUnionBlockSuffix s)
  where
    f p ('|':xs)  = Branch (bracketTree p) '+' (unionTree xs)
    f p _         = (bracketTree p)


regexpToBTree xs = unionTree xs
