{-
Multiples of 3 and 5
Problem 1

If we list all the natural numbers below 10 that are multiples of 3 or 5,
we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
-}

-- どこまで取るか
maxNumber = 999

-- 3 か 5 の倍数を取り出す
takeNumberOfMultiplesOfThreeAndFive :: Int -> Bool
takeNumberOfMultiplesOfThreeAndFive n =
  mod n 3 == 0 || mod n 5 == 0

-- 入力した整数までの 3 か 5 の倍数からなる数のリストを作る
getNumbers :: Int -> [Int]
getNumbers n = filter takeNumberOfMultiplesOfThreeAndFive [1..n]

-- 数のリストを取ってその全ての和を取る
takeSums :: [Int] -> Int
takeSums lst = foldr (+) 0 lst


main = do
  print $ takeNumberOfMultiplesOfThreeAndFive 3
  print $ takeNumberOfMultiplesOfThreeAndFive 5
  print $ takeNumberOfMultiplesOfThreeAndFive 15
  print $ (getNumbers 2 == [])
  print $ (getNumbers 3 == [3])
  print $ (getNumbers 3 == [3])
  print $ (getNumbers 5 == [3, 5])
  print $ (getNumbers 5 == [3, 5])
  print $ (getNumbers 15 == [3, 5, 6, 9, 10, 12, 15])
  print $ (takeSums [] == 0)
  print $ (takeSums [1] == 1)
  print $ (takeSums [1, 2] == 3)
  print $ (takeSums [1, 2, 3] == 6)
  print $ getNumbers maxNumber
  print $ takeSums $ getNumbers maxNumber
