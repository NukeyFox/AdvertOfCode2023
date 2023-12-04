import Data.Text (splitOn, pack, unpack)
import Data.Array (array, (//), (!), elems)
data Card = Card {index :: Int, winning :: [Int], have :: [Int]} deriving (Show)

parseLine line = Card parseCardIndex parseWin parseHave
    where parseCardIndex = (read . unpack . last) $ splitOn (pack " ") meta  :: Int
          parseWin  = map (read . unpack) $ filter (not . null . unpack) (splitOn (pack " ") win)
          parseHave = map (read . unpack) $ filter (not . null . unpack) (splitOn (pack " ") have)
          [win, have] = splitOn (pack "|") mesa
          [meta, mesa] = splitOn (pack ":") (pack line)
readInput filename = do fmap (map parseLine . lines) (readFile filename)

part1_sol = sum . map score
    where score (Card _ win have) = fst $ foldl (\(n,m) x -> if x `elem` win then (m,2*m) else (n,m)) (0,1) have

-- important fact: you never need to backtrack since copies only propagate forward
makeCopies arr (Card i win have)  = arr // [(x, (arr ! i) + (arr ! x)) | x <- [i+1..i+wins]]
    where wins = foldl (\n x -> if x `elem` win then n + 1 else n) 0 have
part2_sol = sum . elems . foldl makeCopies (array (1, 186) (zip [1..186] (repeat 1)))

main = do
    input <- readInput "input.txt"
    print $ part1_sol input
    print $ part2_sol input