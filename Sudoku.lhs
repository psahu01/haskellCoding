1A Simple Sudoku Solver
27th September, 2007
In Chapter 05
_________________________________________________________
0. Basic data types

0.1 Data types

> type Matrix a = [Row a]
> type Row a    = [a]

> type Grid     = Matrix Digit
> type Digit    = Char

> digits  :: [Digit]
> digits  =  ['1'..'9']

> blank   :: Digit -> Bool
> blank   =  (== '0')

0.2 examples of data

0.2a define a tic tac toe grid: ttt

> ttt :: Matrix Char
> ttt = [row1, row2, row3]
> row1, row2, row3 :: [Char]
> row1 = "OXX"
> row2 = "X O"
> row3 = "OXX"

0.2b define a sudoku grid (according to Figure 5.1)

> example51 :: Matrix Digit
> example51 = [
>  "004005700",
>  "000009400",
>  "360000008",
>  "720060000",
>  "000402000",
>  "000080093",
>  "400000056",
>  "005300000",
>  "006100900"]

> example51ez :: Matrix Digit
> example51ez = [
>  "104605709",
>  "570809460",
>  "360041508",
>  "720063140",
>  "953402080",
>  "041080293",
>  "417000356",
>  "005376810",
>  "836100072"]

> example51ez2 :: Matrix Digit
> example51ez2 = [
>  "184625730",
>  "572839461",
>  "369741528",
>  "728963145",
>  "953412687",
>  "641587293",
>  "417298356",
>  "295376814",
>  "836154972"]


0.2c miscellaneous (to be added later)

An example to demonstrate how cp works

> ex4cp :: [[Char]]
> ex4cp =  ["aeiou","ae","io"]

1. Specification (bird: chapter 5, section 1 and 2)

1.1 A brute force approach 

We begin with a brute force approach to develop a solution by 
composing the function solve1, using the following functions
as components:

a. choices
b. expand
c. filter valid

> solve1 :: Grid -> [Grid]
> solve1 = filter valid . expand . choices

1.1a the function choices is defined as:

> type Choices = [Digit]

> choices :: Grid -> Matrix Choices
> choices = map (map choice)
>  where choice d | blank d   = digits
>                 | otherwise = [d]

1.1b the function expand and its auxiliary function 
     (cp: cartesian product), are defined as:

> expand :: Matrix Choices -> [Grid]
> expand = cp . map cp

> cp :: [[a]] -> [[a]]
> cp []       = [[]]
> cp (xs:xss) = [x:ys | x <- xs, ys <- cp xss]


1.1c the function valid, and its auxiliary functions:
     nodups (no duplicates), rows, cols, boxes, group 
     and ungroup are defined as:     

> valid  :: Grid -> Bool
> valid g = all nodups (rows g) &&
>           all nodups (cols g) &&
>           all nodups (boxs g)

In words, valid is to check, for any row, col or box of the proposed
solution, there's no duplicates (i.e. each character is used exactly 
once). 

> nodups       :: Eq a => [a] -> Bool
> nodups []     = True
> nodups (x:xs) = x `notElem` xs && nodups xs

The rows, cols and boxes functions extract the rows (resp. columns, 
boxes) of the sudoku grid into a list of rows (resp. columns, boxes) 
respectively. Note that boxes can be defined using concat (ungroup) 
and group (an `inverse' like operation to ungroup)

> rows :: Matrix a -> [Row a]
> rows = id

> cols          :: Matrix a -> [Row a]
> cols [xs]     = [[x] | x <- xs]
> cols (xs:xss) = zipWith (:) xs (cols xss)

> boxs :: Matrix a -> [Row a]
> boxs = map ungroup . ungroup . map cols .
>        group . map group

> ungroup          = concat
> group []         = []
> group (x:y:z:xs) = [x,y,z]:group xs

2. Pruning 
(bird: chapter 5, section 3. Also see bird, chapter 12, section 8)

2.1 Rationale

A careful examination of the brute force approach suggests
that we may search for a solution by first pruning the choices.
The efficiency of the resulting algorithm can be improved
(better than the brute force approach).

2.2 Pruning

2.2a The pruning step

Recall that, in previous section, the choices function will, take
a sudoku grid as input, return an object C (of type Matrix Choices)
that represent the possible choices to complete the sudoku game.
We will now develop a prune method. As a function, it will apply 
pruning to each row, to each column, and to each box to eliminate 
the choices (indicated in C) that will not lead to a solution.

2.2b Develop a function prune

The function will remove any choices from a cell c that already
occur as a singleton entries in the row column and box containing
c. 

> prune :: Matrix Choices -> Matrix Choices
> prune =
>  pruneBy boxs . pruneBy cols . pruneBy rows
>  where pruneBy f = f . map pruneRow . f

> pruneRow :: Row Choices -> Row Choices
> pruneRow row = map (remove ones) row
>  where ones = [d | [d] <- row]


2.2b The clean up step

> remove :: Choices -> Choices -> Choices
> remove xs [d] = [d]
> remove xs ds  = filter (`notElem` xs) ds

2.2c A new solver

> many     :: (Eq a) => (a-> a) -> a -> a
> many f x = if x == y then x else many f y
>     where y = f x

> solve = filter valid . expand . many prune .choices

