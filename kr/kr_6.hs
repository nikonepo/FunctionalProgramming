import Data.Set (Set, empty, insert, member)
import Data.PQueue.Prio.Min (MinPQueue, empty, insert, findMin, deleteMin)

trickyList :: [Integer]
trickyList = generateTrickyList

generateTrickyList :: [Integer]
generateTrickyList = go empty (insert 1 empty)
  where
    go :: Set Integer -> MinPQueue Integer -> [Integer]
    go seen queue
      | null queue = []
      | otherwise = let
          (x, q') = findMin queue
          q'' = insert (5 * x) (insert (8 * x) q')
          newSeen = insert x seen
        in x : go newSeen q''

-- Tests
main :: IO ()
main = do
    take 12 trickyList