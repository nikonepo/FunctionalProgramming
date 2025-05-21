-- 3) Реализуйте функцию умножения матриц mmult :: (Num a) => [[a]] -> [[a]] -> [[a]].
--    Функция должна работать за полиномиальное от размеров матриц время.
--    Поведение неопределено, если входные списки не имеют нужного вида.

mmult :: (Num a) => [[a]] -> [[a]] -> [[a]]
mmult a b
  | null a || null b || null (head a) || null (head b) = error "Matrix is illegal or empty"
  | length (head a) /= length b = error "Incompatible matrices"
  | otherwise = [[sum $ zipWith (*) rowA colB | colB <- transpose b] | rowA <- a]
  where
    transpose :: [[a]] -> [[a]]
    transpose ([]:_) = []
    transpose x = map head x : transpose (map tail x)

-- Tests
main :: IO ()
main = do
    let a = [[1, 2], [3, 4]]
    let b = [[5, 6], [7, 8]]

    let result = mmult a b

    -- [[19, 22], [43, 50]]
    print result