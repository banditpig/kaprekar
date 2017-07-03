import Control.Monad
import Data.List

toDigs :: Integer -> [Integer]
toDigs = map (read . return) . show

fromDigs :: [Integer] -> Integer
fromDigs = read . join . map show 

sortDesc :: (Ord a ) => [a] -> [a]
sortDesc = sortBy (flip compare)

kap :: Integer -> [Integer]
kap 6174 = [6174]
kap n = n : kap n' where 
    digs = toDigs n 
    n1'  = fromDigs . sortDesc $ digs
    n2'  = fromDigs . sort     $ digs
    n'   = abs (n1' - n2')


kap' :: Integer -> [Integer] -> [Integer]
kap' n ns
 | ns == [] = kap' n [n]
 | n' `elem` ns  = ns
 | otherwise = k
  where
    k    = kap' n'  (ns ++ [n'])         
    digs = toDigs n 
    n1'  = fromDigs . sortDesc $ digs
    n2'  = fromDigs . sort     $ digs
    n'   = abs (n1' - n2')


anyKap n = take 100 $ kap n 