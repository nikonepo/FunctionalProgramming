-- 8) Реализуйте функцию filterM :: Monad m => (a -> m Bool) -> [a] -> m [a],
--    эквивалентную библиотечной на всех допустимых входах, используя лишь функции mapM и liftM,
--    а также любые функции, чьи типы не содержат ограничений вида Monad m, Applicative m или более сильных.

import Control.Monad (liftM)

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM p xs = liftM (map fst . filter snd . zip xs) (mapM p xs)

-- Tests
main :: IO ()
main = do
    result <- filterM (\x -> return (x `mod` 2 == 0)) [1, 2, 3, 4, 5, 6]
    -- [2,4,6]
    print result