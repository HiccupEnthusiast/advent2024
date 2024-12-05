import Data.Maybe (isJust)
import Data.List (union, intersect)

lineLen = 140

main = do
   inp <- readFile "input"
   let len = length . head . lines $ inp
   let h = countOccurrences . lines $ inp
   let v = countOccurrences . mapRows firstToColumn . lines $ inp
   let d0 = (\(a, b)-> countOccurrences a + countOccurrences b) . mapDiagonals . lines $ inp
   let d1 = (\(a, b)-> countOccurrences a + countOccurrences b) . mapDiagonals . map reverse . lines $ inp

   print $ h + v + d0 + d1
   --print . (\(a,b)->(a, b)) . mapDiagonals . lines  $ inp
   print . (\(a,b)->( posCores . genCores $ a, posCores2 . genCores $  b)) . mapDiagonals . lines  $ inp
   print . (\(a,b)->( posCores3 . genCores $ a, posCores4 . genCores $  b)) . mapDiagonals . map reverse . lines  $ inp
   let d2 = (\(a,b)-> a ++ b) . (\(a,b)->( posCores . genCores $ a, posCores2 . genCores $ b)) . mapDiagonals . lines  $ inp
   --print . (\(a, b)->(a,b)) . mapDiagonals . map reverse . lines  $ inp
   let d3 = (\(a, b)->a ++ b). (\(a,b)->( posCores3 . genCores $ a, posCores4 . genCores $ b)) . mapDiagonals . map reverse . lines  $ inp

   print . length $ d2 `intersect` d3


   return ()

countOccurrences::[String] -> Int
countOccurrences = sum . map  (count . windows)

windows::String -> [String]
windows (a:b:c:d:xs) = [a, b, c, d] : windows (b:c:d:xs)
windows _ = []

count::[String] -> Int
count xs = sum [1 | x <- xs, x == "XMAS" || x == reverse "XMAS"]

mapRows::([String] -> String) -> [String] -> [String]
mapRows _ [] = []
mapRows f s@((_:""):_) = [f s]
mapRows f s@((h:r):t) = f s : mapRows f (map tail s)
mapRows _ _ = []

mapDiagonals::[String] -> ([String], [String])
mapDiagonals x = (mapRows generateDiagonal x, tail . mapRows generateDiagonal . mapRows firstToColumn $ x)

firstToColumn::[String] -> String
firstToColumn (h:s) = head h : firstToColumn s
firstToColumn _ = []

generateDiagonal::[String] -> String
generateDiagonal xss@(a:b:c:d:_) = [if i < length xs then xs !! i else '_' | (xs, i) <- zip xss [0..], i < length xs || False]
generateDiagonal _ = ""

genCores::[String] -> [[Maybe String]]
genCores xss = [[if s == "MAS" || s == "SAM" then Just s else Nothing |s<-windows3 xs] | xs<- xss, length xs >= 3]

-- HARCODED LINE LENGTH, FIX
posCores::[[Maybe String]] -> [Int]
posCores xss = concat [[(lineLen*i+l+i)+(lineLen+2) | (x, i) <- zip xs [0..], isJust x]
                  | (xs, l) <- zip xss [0..]]

posCores2::[[Maybe String]] -> [Int]
posCores2 xss = concat [[(lineLen*i+i)+l*lineLen + (lineLen*2+2) | (x, i) <- zip xs [0..], isJust x]
                  | (xs, l) <- zip xss [0..]]

posCores3::[[Maybe String]] -> [Int]
posCores3 xss = concat [[(lineLen*(i+1))-(i+l)+(lineLen-1) | (x, i) <- zip xs [0..], isJust x]
                  | (xs, l) <- zip xss [0..]]

posCores4::[[Maybe String]] -> [Int]
posCores4 xss = concat [[lineLen*(i+3)-1-i+l*lineLen| (x, i) <- zip xs [0..], isJust x]
                  | (xs, l) <- zip xss [0..]]

windows3::String -> [String]
windows3 (a:b:c:xs) = [a, b, c] : windows3 (b:c:xs)
windows3 _ = []
