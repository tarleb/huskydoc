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
Module      :  Text.Huskydoc.Table
Copyright   :  © 2016 Albert Krewinkel
License     :  ISC

Maintainer  :  Albert Krewinkel <tarleb@zeitkraut.de>
Stability   :  experimental
Portability :  portable

Parsers for tables and their components
-}
module Text.Huskydoc.Table
  ( table
  , tableCell
  , tableRow
  , tableBoundary
  ) where

import Text.Huskydoc.Inlines ( inlinesWithinLine )
import Text.Huskydoc.Parsing
import Text.Huskydoc.Patterns

-- | Parse a table
table :: Parser (Attributes -> BlockElement)
table = try $ do
  tableBoundary
  firstRow <- tableRow
  bodyRows <- manyTill tableRow tableBoundary
  return (\a -> RichTable a (Just firstRow) bodyRows Nothing)

-- | Parse a default table boundary
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
