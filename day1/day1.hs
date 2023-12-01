{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Char (isDigit)
import Data.Text (stripPrefix, tail, head, pack, cons)
getCalibrationValue line = read . (\x -> if null x then "0" else [Prelude.head x, last x]) $ filter isDigit line :: Int
part1_sol = sum . map getCalibrationValue
--a bit hacky but it gets the job done..
replacePrefix (stripPrefix "one"   -> Just restOfString) = ('1',  cons 'e' restOfString)
replacePrefix (stripPrefix "two"   -> Just restOfString) = ('2',  cons 'o' restOfString)
replacePrefix (stripPrefix "three" -> Just restOfString) = ('3',  cons 'e' restOfString)
replacePrefix (stripPrefix "four"  -> Just restOfString) = ('4',           restOfString)
replacePrefix (stripPrefix "five"  -> Just restOfString) = ('5',  cons 'e' restOfString)
replacePrefix (stripPrefix "six"   -> Just restOfString) = ('6',           restOfString)
replacePrefix (stripPrefix "seven" -> Just restOfString) = ('7',  cons 'n' restOfString)
replacePrefix (stripPrefix "eight" -> Just restOfString) = ('8',  cons 't' restOfString)
replacePrefix (stripPrefix "nine"  -> Just restOfString) = ('9',  cons 'e' restOfString)
replacePrefix string = (Data.Text.head string,  Data.Text.tail string)
replaceAll "" = ""
replaceAll str = a : replaceAll b
    where (a, b) = replacePrefix str
part2_sol = part1_sol . map (replaceAll . pack)
readInput filename = do
    fmap lines (readFile filename)
main = do
    input <- readInput "input.txt"
    print $ part1_sol input
    print $ part2_sol input

    