module Homework03 where

import Json


{----------------------------------------------------------------

Task: Implement the parser below.

Note that this parser does not correspond to real JSON arrays:

* real JSON arrays are comma-delimited (not space-delimited)
* real JSON arrays can contain more than just numbers

But this will serve as good practice before we get to the real thing.

This homework can be solved by using the existing parsers and
combinators, without using the low-level details of Parser.

Hint: use the "token" combinator to wrap other parsers to make the
handling of spaces more seamless.


----------------------------------------------------------------}


numArray :: Parser [Int]
numArray = undefined


test :: Bool
test =
    Just ([1,2,3,4], "") == run numArray "  [ 1 2   3  4 ] "
