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
  , RawAttributes
  , RawAttr (..)
  , attribValue
  , attributes
  , blockRawAttributes
  , blockTitle
  , fromRawAttributes
  , imageAttributes
  , namedAttr
  , positionalAttr
  , positionalsToAttrs
  , rawAttributes
  , toAttributes
  ) where

import Data.Monoid ( (<>) )
import Data.List ( find, partition )
import Data.Text ( Text, pack, strip )
import Text.Huskydoc.Parsing
import Text.Huskydoc.Types

attributes :: Parser Attributes
attributes = fromRawAttributes <$> rawAttributes

rawAttributes :: Parser RawAttributes
rawAttributes = label "raw attributes" $
  let parseAttrs = (namedAttr <|> positionalAttr) `sepBy` (comma *> skipSpaces)
  in try (between (char '[') (char ']') parseAttrs)

imageAttributes :: Parser Attributes
imageAttributes =
  let parseAttrs = (namedAttr <|> positionalAttr) `sepBy` (comma *> skipSpaces)
      removeStyle = filter ((/= "style") . attrKey)
  in withAttrs removeStyle . fromRawAttributes . (PositionalAttr "image":)
     <$> try (between (char '[') (char ']') parseAttrs)

-- | Helper function, applying a filter to Attributes
withAttrs :: ([Attr] -> [Attr]) -> Attributes -> Attributes
withAttrs f = toAttributes . f . fromAttributes

attribValue :: Attributes -> Text -> Maybe Text
attribValue attribs key =
  attrValue <$> find ((== key) . attrKey) (fromAttributes attribs)

-- | Set of raw attrs
type RawAttributes = [RawAttr]

-- | Raw, uninterpreted attributes as they are given in the text.
data RawAttr =
    NamedAttr Text Text
  | StyleAttr Text
  | PositionalAttr Text
  | OptionRawAttr Text
  deriving (Show, Eq, Ord)

simpleNamedAttr :: Text -> Text -> RawAttr
simpleNamedAttr k v = NamedAttr k v

fromRawAttributes :: RawAttributes -> Attributes
fromRawAttributes raws =
  let (positionals, nonPositionals) = partition isPositionalAttr raws
  in toAttributes $
       (positionalsToAttrs positionals) <> (foldr go [] nonPositionals)
  where
    go a acc = case a of
      (NamedAttr key val) -> (Attr key val) : acc
      (StyleAttr style)   -> (Attr "style" style) : acc
      (PositionalAttr _)  -> acc
      _                   -> acc

isPositionalAttr :: RawAttr -> Bool
isPositionalAttr (PositionalAttr _) = True
isPositionalAttr _                  = False

positionalsToAttrs :: [RawAttr] -> [Attr]
positionalsToAttrs as =
  let values = foldr keepPositionals [] as
  in ($ values) $ case values of
    ("quote":_)  -> zipWith Attr ["style", "attribution", "citetitle"]
    ("verse":_)  -> zipWith Attr ["style", "attribution", "citetitle"]
    ("source":_) -> zipWith Attr ["style", "language", "linenums"]
    ("image":_)  -> zipWith Attr ["style", "alt", "width", "height"]
    _            -> const []
  where
    keepPositionals :: RawAttr -> [Text] -> [Text]
    keepPositionals ra acc = case ra of
      (PositionalAttr txt) -> txt:acc
      _                    -> acc

namedAttr :: Parser RawAttr
namedAttr = try $ do
  name <- pack <$> alphaNumChar `manyTill` (skipSpaces *> char '=')
  value <- pack <$> between doubleQuote doubleQuote (some (noneOf ("\"]"::String)))
  return $ simpleNamedAttr name value

positionalAttr :: Parser RawAttr
positionalAttr = PositionalAttr . strip . pack <$> try (some (noneOf ("],"::String)))

-- | Title of a block element
blockTitle :: Parser RawAttr
blockTitle = NamedAttr "title" . pack <$> try title
  where title = char '.' *> notFollowedBy spaceChar *> someTill anyChar eol

blockId :: Parser RawAttr
blockId = NamedAttr "id" . pack <$> try identifier
  where identifier = between (string "[[")
                             (string "]]")
                             (some (alphaNumChar <|> oneOf ("-_"::String)))
                     <* skipSpaces <* eol

-- | All raw attributes of a block
blockRawAttributes :: Parser RawAttributes
blockRawAttributes = try $ do
  attrList <- some $ choice [ (:[]) <$> blockTitle
                            , (:[]) <$> blockId
                            , rawAttributes <* skipSpaces <* eol
                            ]
  return $ mconcat attrList

comma :: Parser Char
comma = char ','

doubleQuote :: Parser Char
doubleQuote = char '"'
