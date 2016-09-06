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
  , horizontalRule
  , bulletList
  , paragraph
  , sectionTitle
  -- helpers
  , withBlockAttributes
  , bulletListItem
  ) where

import Control.Applicative ( (<**>) )
import Control.Monad ( guard, mzero, void )
import Data.List ( findIndex )
import Data.Maybe ( fromMaybe )
import Text.Huskydoc.Attributes ( blockAttributes )
import Text.Huskydoc.Inlines ( inlines, inlinesExcluding
                             , InlineParser(..) )
import Text.Huskydoc.Parsing
import Text.Huskydoc.Patterns

blocks :: Parser Blocks
blocks = toBlocks <$> some blockElement

blockElement :: Parser BlockElement
blockElement =
  many blankline *>
  (fromMaybe mempty <$> optional blockAttributes) <**> choice
    [ bulletList
    , horizontalRule
    , sectionTitle
    , paragraph
    ] <?> "blocks"

withBlockAttributes :: Parser (Attributes -> BlockElement) -> Parser BlockElement
withBlockAttributes p = fromMaybe mempty <$> optional blockAttributes <**> p

-- | Parse an horizontal rule
horizontalRule :: Parser (Attributes -> BlockElement)
horizontalRule = RichHorizontalRule <$ try (choice ruleParsers <* blankline)
  where ruleParsers = [ string "---" , string "- - -"
                      , string "***" , string "* * *"]

-- | Parse a bullet list
bulletList :: Parser (Attributes -> BlockElement)
bulletList = try $ do
  marker <- bulletListItemMarker
  firstItem <- bulletListItem ""
  flip RichBulletList . (firstItem:) <$> many (bulletListItem marker)

-- | Parse a list element marked by @marker@
bulletListItem :: String -> Parser ListItem
bulletListItem marker = try $ do
  string marker <* someSpaces
  element <- blockElement <* optional eol
  return . ListItem . (:[]) $ element

-- | Parse the start marker of a list item
bulletListItemMarker :: Parser String
bulletListItemMarker = try $ do
  first <- oneOf bulletListMarkerChars
  rest <- many (char first)
  return (first : rest)

-- | Characters that can be used to mark a list item
bulletListMarkerChars :: String
bulletListMarkerChars = "-*"


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
  inlns <- inlinesExcluding [SoftBreakParser, HardBreakParser]
  return $ \a -> RichSectionTitle a level inlns

-- | Parse an underlined section title
underlinedSectionTitle :: Parser (Attributes -> BlockElement)
underlinedSectionTitle = try $ do
  let titleLine = inlinesExcluding [SoftBreakParser, HardBreakParser]
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

-- | A simple paragraph
paragraph :: Parser (Attributes -> BlockElement)
paragraph = try $ do
  _ <- skipMany blankline
  contents <- inlines
  return $ flip RichParagraph contents
