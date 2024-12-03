main = do
  print . count .  map (val 0 . dists . pair . parseReport) . lines =<< readFile "input"
  print . count .  map (val 1 . dists . pair . parseReport) . lines =<< readFile "input"

parseReport::String -> [Int]
parseReport = map read . words

pair::[Int] -> [(Int, Int)]
pair [] = []
pair xs = zip xs (tail xs)

dists::[(Int, Int)] -> [Int]
dists xs = [l-r | (l,r) <- xs]

val::Int -> [Int] -> Bool
val tol xs = (distErrCount xs + dirErrCount xs) <= tol

distErrCount::[Int] -> Int
distErrCount xs = sum [1 | x <- xs, let y = abs x, y>3 || y<=0]

dirErrCount::[Int] -> Int
dirErrCount [] = 0
dirErrCount (x:xs) = 
  let neg = x < 0
  in if neg then 
    sum [1 | x <- xs, x>0]
    else 
    sum [1 | x <- xs, x<0]

valDir::[Int] -> Bool
valDir (x:xs)
  | x < 0     = all (<0) xs
  | otherwise = all (>0) xs

count::[Bool] -> Int
count xs = sum [1 | x<-xs, x]

