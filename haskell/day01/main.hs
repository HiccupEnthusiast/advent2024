import Data.List

main = do
  print . sum . pairDist . ordPair  . split . map parseInt . words =<< readFile "input"
  print . sum . distScore           . split . map parseInt . words =<< readFile "input"

parseInt::String -> Int
parseInt = read

split::[Int] -> ([Int], [Int]) 
split s = 
  ([x | (x, i) <- zip s [0..], even i], 
    [x | (x, i) <- zip s [0..], odd i])

--
ordPair::([Int], [Int]) -> ([Int], [Int])
ordPair (l,r) = (sort l, sort r)

pairDist::([Int], [Int]) -> [Int]
pairDist (l,r) = [ abs $ l-r | (l, r) <- zip l r]
--
distScore::([Int], [Int]) -> [Int]
distScore (xs,ys)  = [ x * countOccurrences x ys | x <-xs]
--
countOccurrences::Int -> [Int] -> Int
countOccurrences x xs = sum [1 | y <- xs, y == x]
