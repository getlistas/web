-- @reference https://github.com/rnons/purescript-html-parser-halogen/blob/e0db4721fa18fa461507dde73a979fa0409970d7/src/Html/Parser.purs
module Html.Parser
  ( HtmlNode(..)
  , Element(..)
  , HtmlAttribute(..)
  , parse
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (defer)
import Data.Array as Array
import Data.Either (Either)
import Data.List (List)
import Data.List as List
import Data.String.CodeUnits (fromCharArray)
import Text.Parsing.StringParser (Parser, ParseError, runParser, try)
import Text.Parsing.StringParser.CodeUnits (anyChar, regex, skipSpaces, string, whiteSpace)
import Text.Parsing.StringParser.Combinators (many, manyTill, option, optional, sepEndBy)

data HtmlNode
  = HtmlElement Element
  | HtmlText String
  | HtmlComment String

derive instance eqHtmlNode :: Eq HtmlNode

type Element =
  { name :: String
  , attributes :: List HtmlAttribute
  , children :: List HtmlNode
  }

data HtmlAttribute = HtmlAttribute String String

derive instance eqHtmlAttribute :: Eq HtmlAttribute

mkElement :: String -> List HtmlAttribute -> List HtmlNode -> Element
mkElement =
  { name: _
  , attributes: _
  , children: _
  }

charListToString :: List Char -> String
charListToString = fromCharArray <<< Array.fromFoldable

attributeParser :: Parser HtmlAttribute
attributeParser = do
  k <- regex "[^=>/ ]+"
  void whiteSpace
  v <- option "" (equals *> quotedString)
  pure $ HtmlAttribute k v

equals :: Parser String
equals =
  string "=" <* whiteSpace

quotedString :: Parser String
quotedString =
  quotedString1 <|> quotedString2

quotedString1 :: Parser String
quotedString1 =
  string "'" *> regex "[^']*" <* string "'"

quotedString2 :: Parser String
quotedString2 =
  string "\"" *> regex "[^\"]*" <* string "\""

openingParser :: Parser Element
openingParser = do
  _ <- string "<"
  tagName <- regex "[^/>\n ]+"
  attributes <- whiteSpace *> sepEndBy attributeParser whiteSpace
  pure $ mkElement tagName attributes List.Nil

selfClosingTags :: Array String
selfClosingTags =
  [ "br", "img", "hr", "meta", "input", "embed", "area", "base", "col"
  , "keygen", "link", "param", "source", "command", "link", "track", "wbr"
  ]

isSelfClosingElement :: Element -> Boolean
isSelfClosingElement ele = ele.name `Array.elem` selfClosingTags

closingOrChildrenParser :: Element -> Parser Element
closingOrChildrenParser element = defer \_ ->
  if isSelfClosingElement element
  then whiteSpace *> optional (string "/") *> string ">" *> pure element
  else
    if element.name == "script"
    then scriptParser
    else childrenParser
  where
    scriptParser = do
      _ <- whiteSpace *> string ">"
      content <- manyTill anyChar (string "</script>")
      pure $ element
        { children = List.singleton $ HtmlText $ charListToString content }
    childrenParser = do
      _ <- whiteSpace *> string ">"
      children <- manyTill nodeParser
                 (string ("</" <> element.name <> ">"))
      pure $ element { children = children }

elementParser :: Parser HtmlNode
elementParser = defer \_ -> do
  skipSpaces
  openingParser >>=
    closingOrChildrenParser >>=
    pure <<< HtmlElement

foreign import decodeHtmlEntity :: String -> String

textParser :: Parser HtmlNode
textParser = HtmlText <<< decodeHtmlEntity <$> regex "[^<]+"

commentParser :: Parser HtmlNode
commentParser = do
  skipSpaces
  comment <- string "<!--" *> manyTill anyChar (string "-->")
  pure $ HtmlComment $ charListToString comment

nodeParser :: Parser HtmlNode
nodeParser = defer \_ ->
  try textParser <|>
  try commentParser <|>
  elementParser

parse :: String -> Either ParseError (List HtmlNode)
parse input =
  runParser (many nodeParser) input
