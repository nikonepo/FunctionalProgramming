-- 5) Перепишите код
--    f :: [[Int]]-> [Int]
--    f xss = [ x | xs <- xss, let l = length xs, x <- xs, even (x + l) ]
--    через оператор связывания (>>=). Символ <- должен отсутствовать в результате.

f :: [[Int]] -> [Int]
f xss = xss >>= \xs -> let l = length xs in filter (\x -> even (x + l)) xs