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
{-# LANGUAGE OverloadedLists #-}
{-|
Module      :  Text.Huskydoc.Blocks
Copyright   :  © 2016 Albert Krewinkel
License     :  ISC

Maintainer  :  Albert Krewinkel <tarleb@zeitkraut.de>
Stability   :  experimental
Portability :  portable

Parsers for block elements
-}
module Text.Huskydoc.Blocks
  ( blockElement
  , blocks
  -- individual block parsers
  , bulletList
  , horizontalRule
  , orderedList
  , paragraph
  , sectionTitle
  , source
  , table
  -- helpers and intermediary parsers
  , listItem
  , tableCell
  , tableRow
  , withBlockAttributes
  , withRawAttributes
  ) where

import Control.Monad ( guard, mzero, void, when )
import Data.List ( findIndex )
import Data.Maybe ( fromMaybe )
import Data.Monoid ( (<>) )
import Data.Text ( pack )
import GHC.Exts ( IsList(..) )
import Text.Huskydoc.Attributes ( RawAttributes
                                , blockRawAttributes , fromRawAttributes )
import Text.Huskydoc.Inlines ( inlines, inlinesWithinLine )
import Text.Huskydoc.Parsing
import Text.Huskydoc.Patterns

blocks :: Parser Blocks
blocks = fromList <$> some blockElement

blockElement :: Parser BlockElement
blockElement = label "block element" $ many blankline
  *> withRawAttributes (choice . blockElementParsers)
  <* optional eol

blockElementParsers :: RawAttributes -> [Parser BlockElement]
blockElementParsers rawAttrbs = map (<*> pure (fromRawAttributes rawAttrbs))
  [ list
  , horizontalRule
  , sectionTitle
  , source
  , table
  , paragraph
  ]

withRawAttributes :: (RawAttributes -> Parser a) -> Parser a
withRawAttributes p =
  (fromMaybe mempty <$> optional blockRawAttributes) >>= p

withBlockAttributes :: Parser (Attributes -> a) -> Parser a
withBlockAttributes p =
  withRawAttributes $ \x -> p <*> pure (fromRawAttributes x)

-- | Parse an horizontal rule
horizontalRule :: Parser (Attributes -> BlockElement)
horizontalRule = label "horizontal rule" $
  RichHorizontalRule <$ try (choice ruleParsers <* blankline)
  where
    ruleParsers :: [Parser String]
    ruleParsers = [ string "---" , string "- - -"
                  , string "***" , string "* * *"]


--
-- Lists
--

-- | Parse any kind of list block
list :: Parser (Attributes -> BlockElement)
list = bulletList <|> orderedList <?> "any list block"

-- | Parse an ordered list
orderedList :: Parser (Attributes -> BlockElement)
orderedList = label "ordered list" . try $ do
  marker <- skipSpaces *> orderedListItemMarker
  guard . not =<< isInContext (ListContextWithMarker marker)
  withContext (ListContextWithMarker marker) $ do
    -- First item is parsed without a marker, it has been read already
    firstItem <- listItem mempty
    flip RichOrderedList . (firstItem:) <$> many (listItem marker)

-- | Parse a bullet list
bulletList :: Parser (Attributes -> BlockElement)
bulletList = label "bullet list" . try $ do
  marker <- skipSpaces *> bulletListItemMarker
  guard . not =<< isInContext (ListContextWithMarker marker)
  withContext (ListContextWithMarker marker) $ do
    -- First item is parsed without a marker, it has been read already
    firstItem <- listItem mempty
    flip RichBulletList . (firstItem:) <$> many (listItem marker)

-- | Parse the start marker of a list item
bulletListItemMarker :: Parser String
bulletListItemMarker = label "bullet-list item marker" . try $ do
  first <- oneOf bulletListMarkerChars
  rest <- many (char first)
  return (first : rest)

-- | Parse the start marker of an ordered list item
orderedListItemMarker :: Parser String
orderedListItemMarker = label "ordered-list item marker" . try $
  some (char '.')

-- | Characters that can be used to mark a list item
bulletListMarkerChars :: String
bulletListMarkerChars = "-*"

-- | Parse a list element marked by @marker@
listItem :: String -> Parser ListItem
listItem marker = label ("list item with marker '" <> marker <> "'") . try $
  -- The first skipSpaces is conditional as it would consume all spaces if the
  -- marker is empty, causing the next someSpaces to fail.
  skipMany blankline
  *> when (marker /= "") skipSpaces
  *> string marker
  *> someSpaces
  *> (ListItem <$> listItemBlocks)

listItemBlocks :: Parser Blocks
listItemBlocks = label "list item blocks" . try $
  fromList <$> ((:) <$> (paragraph <*> pure nullAttributes <* optional eol)
                    <*> many (continuationBlock <|>
                              (list <*> pure nullAttributes)))

continuationBlock :: Parser BlockElement
continuationBlock = label "continuation block" . try $
  listContinuation *> blockElement <* optional eol

listContinuation :: Parser ()
listContinuation = label "list continuation" . try . void $
  char '+' <* eol


--
-- Section titles
--

-- | Parse a section title
sectionTitle :: Parser (Attributes -> BlockElement)
sectionTitle = try $
      prefixedSectionTitle '='
  <|> prefixedSectionTitle '#'
  <|> underlinedSectionTitle

-- | Parse a section title defined using prefix characters
prefixedSectionTitle :: Char -> Parser (Attributes -> BlockElement)
prefixedSectionTitle c = try $ do
  level <- (\n -> n - 1) . length <$> some (char c)
  someSpaces
  guard (level <= 5)
  inlns <- inlinesWithinLine
  eol
  return $ \a -> RichSectionTitle a level inlns

-- | Parse an underlined section title
underlinedSectionTitle :: Parser (Attributes -> BlockElement)
underlinedSectionTitle = try $ do
  let titleLine = inlinesWithinLine
  (strlen, inlns) <- withColumnCount titleLine <* eol
  lineChar <- titleUnderline strlen
  level <- levelFromLineChar lineChar
  return $ \a -> RichSectionTitle a level inlns

-- | Parse a title underline of length @n@ ± 1
titleUnderline :: Int -> Parser Char
titleUnderline n = try $ do
  let lineChar = oneOf titleUnderlineChars
  firstChar <- lineChar
  count (n-2) lineChar
  optional lineChar
  optional lineChar
  skipSpaces *> void eol
  return firstChar

levelFromLineChar :: Char -> Parser Int
levelFromLineChar c =
  case findIndex (== c) titleUnderlineChars of
    (Just n) -> return n
    Nothing  -> mzero

-- | Characters allowed as underlines for titles, given in descending importance
-- of the underlined header.
titleUnderlineChars :: String
titleUnderlineChars = "=-~^"


--
-- Delimited blocks
--

source :: Parser (Attributes -> BlockElement)
source = label "source block" . try $ do
  delim <- (++) <$> string "----" <*> many (char '-')
  skipSpaces <* void eol
  sourceLines <- manyTill sourceBlockLine (string delim *> skipSpaces *> void eol)
  return $ flip RichSource sourceLines

sourceBlockLine :: Parser SourceLine
sourceBlockLine = label "source line" . try $ do
  SourceLine . pack <$> manyTill anyChar eol


--
-- Tables
--

-- | Parse a table
table :: Parser (Attributes -> BlockElement)
table = try $ flip RichTable <$>
  (tableBoundary *> some tableRow <* tableBoundary)

tableBoundary :: Parser String
tableBoundary = try $
  string "|===" <* optional (many (char '=')) <* skipSpaces <* eol

-- | Parse a simple, single line, table row
tableRow :: Parser TableRow
tableRow = try $ do
  notFollowedBy tableBoundary
  char '|'
  cells <- tableCell `sepBy1` (char '|')
  skipSpaces <* eol
  return $ TableRow cells

-- | Parse a single table cell
tableCell :: Parser TableCell
tableCell =
  let cellInlines = stripInlines . mconcat <$> try
                    (withContext TableContext $ some inlinesWithinLine)
  in (\inlns -> TableCell [Paragraph inlns]) <$> cellInlines

-- | A simple paragraph
paragraph :: Parser (Attributes -> BlockElement)
paragraph = try $ do
  _ <- skipMany blankline
  contents <- inlines
  return $ flip RichParagraph contents
