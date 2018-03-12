module Xml where

import Control.Applicative (many, some, (<|>), liftA3)
import qualified Text.Parsec as P


type Parser = P.Parsec String ()

data Xml
    = Node String [Xml]
    | Text String
    deriving (Eq, Show)

{-
Cheatsheet:

  lift0: pure, return
  lift1: fmap, (<$>)
  lift2: liftA2, liftM2
  andThen: (*>), (>>)
-}


xml :: Parser Xml
xml = P.try xmlNode <|> xmlText


xmlNode :: Parser Xml
xmlNode = do
    name <- openTag
    content <- many xml
    endTag name
    pure (Node name content)


xmlText :: Parser Xml
xmlText =
    Text <$> some (P.noneOf "<>")


openTag :: Parser String
openTag = do
    P.char '<'
    tag <- some P.letter
    P.char '>'
    pure tag


endTag :: String -> Parser ()
endTag name = do
    P.char '<'
    P.char '/'
    P.string name
    P.char '>'
    pure ()


test = P.parseTest xml

result =
    test "<message>Hello <bold>World</bold></message>"


----------------------------------------------------------------
-- Definitions that are more similar to what we did
----------------------------------------------------------------

openTag' :: Parser String
openTag' =
    liftA3 (\_ x _ -> x)
        (P.char '<')
        (many P.letter)
        (P.char '>')


endTag' :: String -> Parser ()
endTag' name =
              P.char '<'
    `andThen` P.char '/'
    `andThen` P.string name
    `andThen` P.char '>'
    `andThen` pure ()

andThen = (*>)
