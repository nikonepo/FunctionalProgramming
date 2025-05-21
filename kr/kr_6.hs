merge :: Ord a => [a] -> [a] -> [a]
merge xs@(x:xt) ys@(y:yt) = case compare x y of
    LT -> x : merge xt  ys
    GT -> y : merge xs  yt
    EQ -> x : merge xt  yt

trickyList :: [Integer]
trickyList = xs
  where
    xs = 1
      : merge (map (5*) xs)
              (map (8*) xs)

-- Tests
main :: IO ()
main = print (take 12 trickyList)