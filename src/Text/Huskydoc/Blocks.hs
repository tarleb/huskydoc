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
  , paragraph
  , sectionTitle
  ) where

import           Control.Monad ( guard, mzero, void )
import           Data.List ( findIndex )
import           Text.Huskydoc.Attributes
import qualified Text.Huskydoc.Builders as B
import           Text.Huskydoc.Inlines ( inlines, inlinesExcluding
                                       , InlineParser(..))
import           Text.Huskydoc.Parsing
import           Text.Huskydoc.Types

blocks :: Parser Blocks
blocks = B.toBlocks <$> some blockElement

blockElement :: Parser BlockElement
blockElement = choice
  [ horizontalRule
  , sectionTitle
  , paragraph
  ] <?> "blocks"

-- | Parse an horizontal rule
horizontalRule :: Parser BlockElement
horizontalRule = B.horizontalRule <$ try (choice ruleParsers <* blankline)
  where ruleParsers = [ string "---" , string "- - -"
                      , string "***" , string "* * *"]

-- | Parse a section title
sectionTitle :: Parser BlockElement
sectionTitle = try $
       prefixedSectionTitle '='
   <|> prefixedSectionTitle '#'
   <|> underlinedSectionTitle

-- | Parse a section title defined using prefix characters
prefixedSectionTitle :: Char -> Parser BlockElement
prefixedSectionTitle c = try $ do
  many blankline
  level <- (\n -> n - 1) . length <$> some (char c)
  someSpaces
  guard (level <= 5)
  inlns <- inlinesExcluding [SoftBreakParser, HardBreakParser]
  return . plainElement $ SectionTitle level inlns

-- | Parse an underlined section title
underlinedSectionTitle :: Parser BlockElement
underlinedSectionTitle = try $ do
  many blankline
  let titleLine = inlinesExcluding [SoftBreakParser, HardBreakParser]
  (strlen, inlns) <- withColumnCount titleLine <* eol
  lineChar <- titleUnderline strlen
  level <- levelFromLineChar lineChar
  return . plainElement $ SectionTitle level inlns

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
paragraph :: Parser BlockElement
paragraph = try $ do
  _ <- skipMany blankline
  attributes <- optional parseAttributes
  contents <- inlines
  return $ contents `B.paragraphWith'` attributes
