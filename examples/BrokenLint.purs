module BrokenLint where

import Prelude

baz :: Int -> Int
baz x = if x == 2 then
  0
    else
  1

foo :: Int -> Int
foo x = if x == 5 then
  bar x
    else
  bar (x + 1)
  where
  bar = (*) 2

-- else is being pushed too far back when linted