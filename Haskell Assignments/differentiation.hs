import Data.List.Split
import Data.List

-- current thought is that expressions will be passed in the format
-- "+ 52x4 + 5x3 - 8x2 + 9"
-- ["+","52x4","+","5x3","-","8x2","+","9"]

isInteger :: Char -> Bool
isInteger x
    | x == '0'  = True
    | x == '1'  = True
    | x == '2'  = True
    | x == '3'  = True
    | x == '4'  = True
    | x == '5'  = True
    | x == '6'  = True
    | x == '7'  = True
    | x == '8'  = True
    | x == '9'  = True
    | otherwise = False

isConstant :: [Char] -> Bool
isConstant [] = False
isConstant (x:xs)
    | isInteger x && length xs > 0  = isConstant xs
    | isInteger x && length xs == 0 = True
    | otherwise                     = False

-- this predicate tells us whether a term is a variable named x
isVariable :: [Char] -> Bool
isVariable x
    | x == "x"     = True
    | otherwise    = False

-- determines if a term is a linear term
isLinearTerm :: [Char] -> Bool
isLinearTerm [] = False
isLinearTerm x
    | length a == 2 && isConstant (a !! 0) && not (isConstant (a !! 1)) = True
    | otherwise                                                         = False
  where a = splitOn "x" x

-- determines if a term is a term to some power
isPowerTerm :: [Char] -> Bool
isPowerTerm [] = False
isPowerTerm x
    | length a == 2 && isConstant (a !! 0) && isConstant (a !! 1) = True
    | otherwise                                                   = False
  where a = splitOn "x" x

-- this predicate defines a "term in x" as either a constant, the variable x, a linear term in x
-- or a term of the form (a x n), where a and n are numbers.
isTerm :: [Char] -> Bool
isTerm [] = False
isTerm x
    | isPowerTerm x  = True
    | isLinearTerm x = True
    | isConstant x   = True
    | isVariable x   = True
    | otherwise      = False

-- checks to see if a char is a valid operator
isOperator :: [Char] -> Bool
isOperator x
    | x == "-"  = True
    | x == "+"  = True
    | x == "/"  = True
    | x == "*"  = True
    | otherwise = False

-- checks to see if a list is a list of terms
isTermList :: [[Char]] -> Bool
isTermList (x:xs)
    | length (x:xs) == 1 && not (isConstant x)   = False
    | length xs == 0                             = True
    | mod (length (x:xs)) 2 == 0 && isOperator x = isTermList xs
    | mod (length (x:xs)) 2 == 1 && isTerm x     = isTermList xs
    | otherwise                                  = False

-- breaks the string into a list than can be parsed and returns whether or not
-- the string is a term list
isPolynomial :: [Char] -> Bool
isPolynomial [] = False
isPolynomial x
    | isTermList a = True
    | otherwise    = False
  where a = splitOn " " x

-- here we define some more meaningfully named functions for handling terms in x of the form (a x n)
getCoefficient :: [Char] -> [Char]
getCoefficient x = head (splitOn "x" x)

getExponent :: [Char] -> [Char]
getExponent x = head (tail (splitOn "x" x))

-- now we implement symbolic derivatives of terms.  Note, the derivative of a term in x with respsect to a variable
-- other than x is 0.  Likewise, the derivative with respect to x of a term in a variable other than x is 0.
trimPowerOne :: [Char] -> [Char]
trimPowerOne x
    | getExponent x == "1" = init x
    | otherwise          = x

calculateDerivative :: [Char] -> [Char]
calculateDerivative x =
    (show ((read (getCoefficient x) :: Integer) * (read (getExponent x) :: Integer))) ++
    "x" ++
    (show ((read (getExponent x) :: Integer) - 1))

dTerm :: [Char] -> [Char]
dTerm x
    | isConstant x   = "0"
    | isVariable x   = "1"
    | isLinearTerm x = getCoefficient x
    | isTerm x       = trimPowerOne (calculateDerivative x)


dTermList :: [Char] -> [Char]
dTermList x
    | x == ""             = ""
    | isOperator (head a) = (head a) ++ " " ++ (dTermList (intercalate " " (tail a)))
    | isTerm (head a)     = (dTerm (head a)) ++ " " ++ (dTermList (intercalate " " (tail a)))
  where a = splitOn " " x
