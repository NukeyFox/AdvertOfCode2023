import Data.Char (isDigit)
import Data.Text (splitOn, pack, unpack)

type Action = (Int, String)
data Game   = Game {game_id :: Int, subsets :: [[Action]]} deriving (Show)
-- I could use regex... or i could do whatever this is.
extractNumPrefix (c:cs)
    | isDigit c = (c : num, rest)
    | otherwise = ("",c:cs)
    where (num, rest) = extractNumPrefix cs
extractNumPrefix cs = ("",cs)
parseGame string
    | isDigit $ head string = Game (read num) (parseLine rest)
    | otherwise = parseGame $ tail string
    where (num, rest) = extractNumPrefix string
parseLine (':':' ':string) = map parseComma (parseSemicolon string)
parseLine _ = []
parseSemicolon = splitOn (pack "; ") . pack
parseComma cubes = map parseRound  (splitOn (pack ", ") cubes)
parseRound cube = (read (unpack a), unpack b)
    where [a,b] = splitOn (pack " ") cube
readInput filename = do
    map parseGame . lines <$> readFile filename

--part 1 sol    
canServe subset to_serve = and [or [c == color && amount >= amt | (amount, color) <- to_serve] | (amt, c) <- subset]
gameStatisfies (Game _ subsets) to_serve = all (`canServe` to_serve) subsets
part1_sol = sum .  map game_id . filter (`gameStatisfies` [(12,"red"),(13,"green"),(14,"blue")])

--part 2 sol 
getCubeMat :: [Int] -> Action -> [Int]
getCubeMat [a,b,c] (n, "red")  = [max a n, b, c]
getCubeMat [a,b,c] (n, "green")= [a, max b n, c]
getCubeMat [a,b,c] (n, "blue") = [a, b, max c n]
getCubeMat x _ = x 
part2_sol = sum . map f
    where f (Game _ subsets) = product $ foldl getCubeMat [0,0,0] (concat subsets)

main = do
    input <- readInput "input.txt"
    print $ part1_sol input
    print $ part2_sol input