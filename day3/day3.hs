import Data.List (groupBy)
import Data.Char (isDigit)
import Data.Set (fromList, toList, filter, size)
data RLE = RLE {string :: String, len :: Int, position :: Int, row :: Int} deriving (Show, Eq, Ord)

runLengthEncoding row = fst
    . foldl (\  (rest, acc) (str, len)-> (rest ++ [RLE str len acc row] , len + acc)) ([], 0)
    . map (\x -> (x, length x))
    . groupBy (\x y -> (isDigit x && isDigit y) || (x == y))
getRLE grid row col = find (grid !! row)
    where find (rle@(RLE string len pos r): xs)
            | col >= pos + len = find xs
            | otherwise        = rle
          find [] = RLE "." 0 0 0
getSym grid row col = head $ string rle where rle = getRLE grid row col
isSymbol x = not (isDigit x) &&  (x /= '.')
numIsPart grid (RLE str len pos row) =
    row     > 0   && or [isSymbol $ getSym grid (row-1) x | x <- [(max 0 (pos-1))..(min 140 (pos+len))]] ||
    row     < 139 && or [isSymbol $ getSym grid (row+1) x | x <- [(max 0 (pos-1))..(min 140 (pos+len))]] ||
    pos     > 0   &&     isSymbol (getSym grid  row (pos-1)) ||
    pos+len < 139 &&     isSymbol (getSym grid  row (pos+len))
solve_part1 grid = sum $ map (sum
                    . map (read . string)
                    . Prelude.filter (\rle -> isDigit (head $ string rle) && numIsPart grid rle))
                    grid
getSurroudingRLE grid (RLE str len pos row) = fromList $
     [getRLE grid (row - 1) x | row > 0,   x <- [(max 0 (pos - 1)) .. (min 140 (pos + len))]] ++
     [getRLE grid  row (pos-1)   | pos > 0]  ++
     [getRLE grid  row (pos+len) | pos <139] ++
     [getRLE grid (row + 1) x | row < 139, x <- [(max 0 (pos - 1)) .. (min 140 (pos + len))]]
getPartNo grid rle = toList $ Data.Set.filter (isDigit . head . string) (getSurroudingRLE grid rle)
solve_part2 grid = sum $ map (product . map (read . string)) 
                   (Prelude.filter (\set -> length set == 2) 
                   (map (getPartNo grid) 
                   (concatMap (Prelude.filter (\rle -> head (string rle) == '*')) grid)))
readInput filename = do fmap (zipWith runLengthEncoding [0..] . lines) (readFile filename)
main = do
    input <- readInput "input.txt"
    print $  solve_part1 input
    print $ solve_part2 input
