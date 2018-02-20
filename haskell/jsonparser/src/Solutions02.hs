module Solutions02 where

import Json (Parser, lift0, lift1, lift2)


{----------------------------------------------------------------

Task: Implement lift3 using only the imports above.  If you need to,
you can write your own helper functions, or use functions from the
Prelude.

Note that we did NOT import the Parser constructor, so you won't be
able to do the homework by using the internal details of the Parser
type.

----------------------------------------------------------------}


lift3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
lift3 f pa pb pc = (lift2 apply) ((lift2 f) pa pb) pc where
  apply :: (c -> d) -> c -> d
  apply g c = g c

-- Test cases

-- l3 = lift3 (\x y z -> x + z) number true  number
-- run l3 "12true34abc" should be Just (46, "abc")