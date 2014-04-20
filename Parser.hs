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
--
-- Provides expansion of [a..b] where a and b respectively are border values
-- e.g. expandRange 'a' 'd' is equivalent to ['a'..'d'] which produces
-- ['a','b','c','d']
--
expandRange :: (Ord a, Enum a) => a -> a -> [a]
expandRange from to =
  if from < to
    then [from .. to]
    else [to .. from]

------------------------------------------------------
--
-- Provides expansion od a body of a set from regular expresion
-- e.g. regular expression set [aA-D0] will be expanded to
-- ['a','A','B','C','D','0']
--
expandList :: [Char] -> [Char]
expandList str = f [] str
  where
    f p []				= p
    f p (x:'-':z:xs)	= (expandRange x z) ++ (f p xs)
    f p (x:xs)			= x : (f p xs)

------------------------------------------------------
--
-- Returns all symbls from an alphabet of allowed symbols from input
--
allowedSymbolSet :: [Char]
allowedSymbolSet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 "

------------------------------------------------------
--
-- Returns all symbols that can be inside of a body of a regular expression set
--
insideSetSymbols :: [Char]
insideSetSymbols = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 -"

------------------------------------------------------
--
-- Transforms given array of symbols to a set of such symbols
-- If the first symbols is '^' then returns complement to all allowed symbols 
--
getSet :: [Char] -> Set.Set Char
getSet [] = Set.empty
getSet ('^':xs) = Set.difference (Set.fromList allowedSymbolSet) (Set.fromList (expandList xs))                 
getSet (x:xs) = Set.fromList (expandList (x:xs))

------------------------------------------------------
--
-- Slave function that seeks for regular expression of the following structure:
-- `(aaaa)bbbb` and return the first par `aaaa`.
--  
getBracketBlockPrefix :: [Char] -> [Char]
getBracketBlockPrefix (x:xs) = f [] xs 1
  where
    f p [] d  | d == 1 = p
              | otherwise = error "Parser: missing ')'."
    f p (x:xs) d  | x == '(' && d > 0  = x : (f p xs (d+1))
                  | x == ')' && d == 1 = p
                  | x == ')' && d > 1  = x : (f p xs (d-1))
                  | otherwise          = x : (f p xs d)

--
-- Slave function that returns the complements of the previous function. Based
-- on given example, it will return `bbbb` part.
--
getBracketBlockSuffix :: [Char] -> [Char]
getBracketBlockSuffix (x:xs) = drop (2 + length (getBracketBlockPrefix (x:xs))) (x:xs)

--------------------------------------------------------
--
-- Slave function that seeks for regular expression of the following structure:
-- `aaaa|bbbb` and return the first par `aaaa`.
-- 
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

--
-- Slave function that returns the complements of the previous function. Based
-- on given example, it will return `bbbb` part.
--
getUnionBlockSuffix :: [Char] -> [Char]
getUnionBlockSuffix (x:xs) = drop (length (getUnionBlockPrefix (x:xs))) (x:xs)

------------------------------------------------------
--
-- This function represents the bottom-level of the Parser.
-- It concatenates single symbols or sets (or . which is an alias for any
-- symbol). Each element can be iterated so an iteration is handled here as well
-- as in middle-level parser.
--
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
--
-- This is the middle-level of the Parser.
-- Parsing parentheses is divided into 2 parts based on given regular expression
-- *	if RE starts with left parentheses then it will find the coresponding
--		right parentheses and it will divide the RE in a such position.
--		Content of the parentheses can be generally anything so it will be
--		parsed by the top-level parser.
-- *	otherwise this parser will divide the given RE when he reach the first
--		left parentheses or the end of RE. This part can contain neither
--		parentheses or alternatives separator so it can be finnaly parsed be the
--		bottom-level parser.
--	RE enclosed in parentheses can be iterated, so iteration is handled here as
--	well as in the bottom-level.
--
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
--
-- This function represents the top-level of the Parser.
-- It separates alternatives based on associativity of an union operation.
-- Regular expression of the following structure `aaaa|bbbb|cccc`` is divided
-- into parts `aaaa` and `bbbb|cccc`.
-- The first part cannot contain more alternatives, so it will go down to the
-- middle-level parser.
-- The second part will be parsed recursively until its length is bigger than 1.
--
unionTree :: [Char] -> BTree
unionTree [] = Leaf Set.empty
unionTree s@(x:xs) = f (getUnionBlockPrefix s) (getUnionBlockSuffix s)
  where
    f p ('|':xs)  = Branch (bracketTree p) '+' (unionTree xs)
    f p _         = (bracketTree p)

------------------------------------------------------
--
-- Main parsing function
-- Parser is divided into 3 levels:
-- *	top-level - parsing alternates (union operation in regular expressions)
-- *	middle-level - parsing parentheses
-- *	bottom-level - parsing concatenation of single symbols or sets
-- Single symbols, sets and also the content of parentheses can be iterated, so
-- middle and bottom levels of the parser parses iteration as well.
--
-- Returns a binary tree that consist of a leaf or a branch that contans 2
-- subtrees and 1 operation (concatenation, iteration or union). Subtree is
-- defined recursively also as a binary tree.
--
regexpToBTree :: [Char] -> BTree
regexpToBTree xs = unionTree xs
