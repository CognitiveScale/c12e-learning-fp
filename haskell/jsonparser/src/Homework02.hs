module Homework02 where

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
lift3 f pa pb pc = undefined
