------------------------------------------------------
-- Project: Simple Grep
-- Authors:
--   xsurov03 - Marek Surovic
--   xstodu05 - Petr Stodulka
--   xpavlu06 - Igor Pavlu
--   xpauli00 - Miroslav Paulik
------------------------------------------------------

module Parser
( BTree(..)
, regexpToBTree
)where

import qualified Data.Set as Set

type Symbol = Set.Set Char

data BTree = Leaf Symbol | Branch BTree Char BTree 
  deriving (Eq,Show)

------------------------------------------------------
expandRange :: (Ord a, Enum a) => a -> a -> [a]
expandRange from to =
  if from < to
    then [from .. to]
    else [to .. from]

------------------------------------------------------
expandList :: [Char] -> [Char]
expandList str = f [] str
  where
    f p []				= p
    f p (x:'-':z:xs)	= (expandRange x z) ++ (f p xs)
    f p (x:xs)			= x : (f p xs)

------------------------------------------------------
allowedSymbolSet :: [Char]
allowedSymbolSet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 "

------------------------------------------------------
insideSetSymbols :: [Char]
insideSetSymbols = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 -"

------------------------------------------------------
getSet :: [Char] -> Set.Set Char
getSet [] = Set.empty
getSet ('^':xs) = Set.difference (Set.fromList allowedSymbolSet) (Set.fromList (expandList xs))                 
getSet (x:xs) = Set.fromList (expandList (x:xs))

------------------------------------------------------  
getBracketBlockPrefix :: [Char] -> [Char]
getBracketBlockPrefix (x:xs) = f [] xs 1
  where
    f p [] d  | d == 1 = p
              | otherwise = error "Parser: missing ')'."
    f p (x:xs) d  | x == '(' && d > 0  = x : (f p xs (d+1))
                  | x == ')' && d == 1 = p
                  | x == ')' && d > 1  = x : (f p xs (d-1))
                  | otherwise          = x : (f p xs d)
getBracketBlockSuffix :: [Char] -> [Char]
getBracketBlockSuffix (x:xs) = drop (2 + length (getBracketBlockPrefix (x:xs))) (x:xs)

------------------------------------------------------
getUnionBlockPrefix :: [Char] -> [Char]
getUnionBlockPrefix (x:xs) = f [] (x:xs) 0
  where
    f p [] d  | d == 0 = p
              | otherwise = error "Parser: missing ')'."
    f p (x:xs) d  | x == '('            = x : (f p xs (d+1))
                  | x == ')' && d < 1   = error "Parser: unexpected ')'."
                  | x == ')'            = x : (f p xs (d-1))
                  | x == '|' && d == 0  = p
                  | otherwise           = x : (f p xs d)

getUnionBlockSuffix :: [Char] -> [Char]
getUnionBlockSuffix (x:xs) = drop (length (getUnionBlockPrefix (x:xs))) (x:xs)

------------------------------------------------------
concatTree :: [Char] -> BTree
concatTree []       = Leaf Set.empty
concatTree (']':xs) = error "Parser: unexpected ']'."
concatTree (x:'*':xs)
  | x == '.'  = Branch (Branch (Leaf (Set.fromList allowedSymbolSet)) '*' (Leaf Set.empty)) '.' (concatTree xs)
  | x `elem` allowedSymbolSet = Branch (Branch (Leaf (Set.singleton x)) '*' (Leaf Set.empty)) '.' (concatTree xs)
concatTree (x:'+':xs)
  | x == '.'  = Branch (Branch (Leaf (Set.fromList allowedSymbolSet)) '.' (Branch (Leaf (Set.fromList allowedSymbolSet)) '*' (Leaf Set.empty))) '.' (concatTree xs)
  | x `elem` allowedSymbolSet = Branch (Branch (Leaf (Set.singleton x)) '.' (Branch (Leaf (Set.singleton x)) '*' (Leaf Set.empty))) '.' (concatTree xs)
concatTree (x:'?':xs)
  | x == '.'  = Branch (Branch (Leaf (Set.fromList allowedSymbolSet)) '+' (Leaf Set.empty)) '.' (concatTree xs)
  | x `elem` allowedSymbolSet = Branch (Branch (Leaf (Set.singleton x)) '+' (Leaf Set.empty)) '.' (concatTree xs)
concatTree ('[':y:xs) =
  if y == '^'
    then f ("^" ++ (takeWhile (`elem` insideSetSymbols) xs)) (dropWhile (`elem` insideSetSymbols) xs)
    else f (takeWhile (`elem` insideSetSymbols) (y:xs)) (dropWhile (`elem` insideSetSymbols) (y:xs)) 
      where
        f p (']':'*':xs)  = Branch (Branch (Leaf (getSet p)) '*' (Leaf Set.empty)) '.' (concatTree xs)
        f p (']':'+':xs)  = Branch (Branch (Leaf (getSet p)) '.' (Branch (Leaf (getSet p)) '*' (Leaf Set.empty))) '.' (concatTree xs)
        f p (']':'?':xs)  = Branch (Branch (Leaf (getSet p)) '+' (Leaf Set.empty)) '.' (concatTree xs)
        f p (']':xs)      = Branch (Leaf (getSet p)) '.' (concatTree xs)
        f _ []            = error "Parser: missing ']'."  
        f _ (x:xs)        = error ("Parser: unexpected symbol '" ++ [x] ++ "'.")
concatTree (x:xs)
  | x == '.'					= Branch (Leaf (Set.fromList allowedSymbolSet)) '.' (concatTree xs)
  | x `elem` allowedSymbolSet	= Branch (Leaf (Set.singleton x)) '.' (concatTree xs)
  | x == '['					= error "Parser: missing ']'."
  | otherwise 					= error ("Parser: unexpected symbol '" ++ [x] ++ "'.")

------------------------------------------------------
bracketTree :: [Char] -> BTree
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

------------------------------------------------------
unionTree :: [Char] -> BTree
unionTree [] = Leaf Set.empty
unionTree s@(x:xs) = f (getUnionBlockPrefix s) (getUnionBlockSuffix s)
  where
    f p ('|':xs)  = Branch (bracketTree p) '+' (unionTree xs)
    f p _         = (bracketTree p)

------------------------------------------------------
regexpToBTree :: [Char] -> BTree
regexpToBTree xs = unionTree xs
