{-
Copyright (c) 2016 Albert Krewinkel

Permission to use, copy, modify, and/or distribute this software for any purpose
with or without fee is hereby granted, provided that the above copyright notice
and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
THIS SOFTWARE.
-}

{-# LANGUAGE OverloadedStrings #-}
{-|
Module      :  Text.Huskydoc.Attribtes
Copyright   :  © 2016 Albert Krewinkel
License     :  ISC

Maintainer  :  Albert Krewinkel <tarleb@zeitkraut.de>
Stability   :  experimental
Portability :  portable

Parser for element attributes.
-}
module Text.Huskydoc.Attributes
    ( Attributes (..)
    , Attr (..)
    , namedAttr
    , parseAttributes
    , positionalAttr
    , toAttributes
    ) where

import Text.Huskydoc.Parsing
import Data.Text

-- | Element attributes
newtype Attributes = Attributes { fromAttributes :: [Attr] }
    deriving (Show, Eq, Ord)

toAttributes :: [Attr] -> Attributes
toAttributes = Attributes

data Attr = NamedAttr Text Text
          | PositionalAttr Text
          | OptionAttr Text
    deriving (Show, Eq, Ord)

simpleNamedAttr :: Text -> Text -> Attr
simpleNamedAttr k v = NamedAttr k v

-- data AttrValue = AttrSimpleValue Text
--                | AttrListValue [Text]
-- roleAttr :: Text -> Attr
-- roleAttr t = simpleNamedAttr "role" t
-- idAttr :: Text -> Attr
-- idAttr t = simpleNamedAttr "id" t
-- optionAttr :: Text -> Attr
-- optionAttr t = simpleNamedAttr "option" t

parseAttributes :: Parser Attributes
parseAttributes =
  let parseAttrs = (namedAttr <|> positionalAttr) `sepBy` (comma *> skipSpaces)
  in toAttributes <$> try (between (char '[') (char ']') parseAttrs)

namedAttr :: Parser Attr
namedAttr = try $ do
  name <- pack <$> alphaNumChar `manyTill` (skipSpaces *> char '=')
  value <- pack <$> between doubleQuote doubleQuote (some (noneOf "\"]"))
  return $ simpleNamedAttr name value

positionalAttr :: Parser Attr
positionalAttr = PositionalAttr . strip . pack <$> try (some (noneOf "],"))

comma :: Parser Char
comma = char ','

doubleQuote :: Parser Char
doubleQuote = char '"'

