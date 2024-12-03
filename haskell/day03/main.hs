import Data.Maybe (catMaybes)
import Text.Read (readMaybe)
main = do
  print . solve . applyIndexes . mulIndexes =<< readFile "input"
  print . solve . addControl True . applyIndexes . bothIndexes =<< readFile "input"


f::([Int], [Int]) -> ([Int], [Int])
f ([], y) = ([], y)
f (x:xs, y:ys)
  | x > y = f (xs, y:x:ys)
  | otherwise = f (xs, y:ys)

solve::[String] -> Int
solve = sum . multPair . catMaybes . parsePair . parsePostfix . parsePrefix

mulIndexes::String -> ([Int], String)
mulIndexes s = ([i + if c == ')' then 1 else 0| (c,i) <- zip s [0..], c == 'm' || c == ')'], s)

bothIndexes::String -> ([Int], String)
bothIndexes s = ([i + if c == ')' then 1 else 0| (c,i) <- zip s [0..], c == 'm' || c == ')' || c == 'd'], s)

windows::[a] -> [(a, a)]
windows [] = []
windows xs = zip xs (tail xs)

applyIndexes::([Int], String) -> [String]
applyIndexes (ix, s) = [substring f l s | (f,l) <- windows ix]

addControl::Bool -> [String] -> [String]
addControl f [] = []
addControl f (s:xs)
  | s == "don't()" = addControl False xs
  | s == "do()" = addControl True xs
  | otherwise = if f then s:addControl f xs else addControl f xs


parsePrefix::[String] -> [String]
parsePrefix xs = map (drop 3) (catMaybes [parseString (startsWith "mul(") x | x <- xs])

parsePostfix::[String] -> [String]
parsePostfix xs = catMaybes [parseString (endsWith ")") x | x <- xs]

parsePair::[String] -> [Maybe (Int, Int)]
parsePair = map readMaybe

multPair::[(Int, Int)] -> [Int]
multPair xs = [a*b | (a,b) <- xs]

startsWith::String -> String -> Bool
startsWith p s =
  let len = length p
      prefix = take len s
  in prefix == p

endsWith::String -> String -> Bool
endsWith p s =
  let len = length p
      postfix = drop (length s - len) s
  in postfix == p

parseString::(String -> Bool) -> String -> Maybe String
parseString f s
  | f s  = Just s
  | otherwise = Nothing


substring::Int -> Int -> String -> String
substring s f = take (f - s) . drop s

