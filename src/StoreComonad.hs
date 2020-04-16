module StoreComonad where

import Control.Comonad.Store
import Control.Monad

main :: IO ()
main = print $ extract test2

test2 :: Store [Int] Int
test2 = s1 =>> \store ->
  applyAdd (runStore (store =>> \store' -> applyAdd (runStore store')))

applyAdd (f, s) = 1 + f s

s1 :: Store [Int] Int
s1 = store head [1, 2, 3]

s2 :: Store [Int] Int
s2 = store last [1, 2, 3]