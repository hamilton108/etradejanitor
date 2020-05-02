module Demo where

import Data.List.Split (splitOn)

x = "a:b:c:x"

lx = splitOn ":" x

y = case lx of
    [v1,v2,v3] -> "Yep"
    _ -> "Nope"