
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
    f p (x:y:z:xs) =
      if y == '-'
        then (expandRange x z) ++ (f p xs)
        else x : (f p (y:z:xs))
    f p (x:xs) = x : (f p xs)


getSet [] = Set.empty
getSet (x:xs) =
  if x == '^'
    then Set.difference (Set.fromList allowedSymbolSet) (Set.fromList (expandList (x:xs)))                 
    else Set.fromList (expandList (x:xs))

allowedSymbolSet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 "


  
getBracketBlockPrefix (x:xs) = f [] xs 1
  where
    f p [] d  | d == 1 = p
              | otherwise = error "REGEXP.parse: Brackets does not match."
    f p (x:xs) d  | x == '(' && d > 0  = x : (f p xs (d+1))
                  | x == ')' && d == 1 = p
                  | x == ')' && d > 1  = x : (f p xs (d-1))
                  | otherwise          = x : (f p xs d)

getBracketBlockSuffix (x:xs) = drop (2 + length (getBracketBlockPrefix (x:xs))) (x:xs)

getUnionBlockPrefix (x:xs) = f [] (x:xs) 0
  where
    f p [] d  | d == 0 = p
              | otherwise = error "REGEXP.parse: Brackets does not match."
    f p (x:xs) d  | x == '('            = x : (f p xs (d+1))
                  | x == ')' && d < 1   = error "REGEXP.parse: Brackets does not match."
                  | x == ')'            = x : (f p xs (d-1))
                  | x == '|' && d == 0  = p
                  | otherwise           = x : (f p xs d)

getUnionBlockSuffix (x:xs) = drop (length (getUnionBlockPrefix (x:xs))) (x:xs)

concatTree [] = Leaf Set.empty
concatTree (x:'*':xs)
  | x == '.'  = Branch (Branch (Leaf (Set.fromList allowedSymbolSet)) '*' (Leaf Set.empty)) '.' (concatTree xs)
  | otherwise = Branch (Branch (Leaf (Set.singleton x)) '*' (Leaf Set.empty)) '.' (concatTree xs)
concatTree (x:'?':xs)
  | x == '.'  = Branch (Branch (Leaf (Set.fromList allowedSymbolSet)) '+' (Leaf Set.empty)) '.' (concatTree xs)
  | otherwise = Branch (Branch (Leaf (Set.singleton x)) '+' (Leaf Set.empty)) '.' (concatTree xs)
concatTree (x:xs)
  | x == '.'  = Branch (Leaf (Set.fromList allowedSymbolSet)) '.' (concatTree xs)
  | x == '['  = f (takeWhile (']'/=) xs) (dropWhile (']'/=) xs)   
  | otherwise = Branch (Leaf (Set.singleton x)) '.' (concatTree xs)
    where
      f p (']':'*':xs)  = Branch (Branch (Leaf (getSet p)) '*' (Leaf Set.empty)) '.' (concatTree xs)
      f p (']':'*':xs)  = Branch (Branch (Leaf (getSet p)) '+' (Leaf Set.empty)) '.' (concatTree xs)
      f p (']':xs)      = Branch (Leaf (getSet p)) '.' (concatTree xs) 

bracketTree [] = Leaf Set.empty
bracketTree s@('(':xs) = f (getBracketBlockPrefix s) (getBracketBlockSuffix s)
  where
    f p ('*':xs) = Branch (Branch (unionTree p) '*' (Leaf Set.empty)) '.' (unionTree xs)
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
