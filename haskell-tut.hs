import Data.List
import System.IO
import Data.Char

powerSet base y = [base^x | x <- [0,1..y-1]]

digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]


{-result :: Int -> Int -> Int-}
myResult = (sum (zipWith (*) (reverse (digs 1233214)) (powerSet 7 7)))

main = do
  print myResult

