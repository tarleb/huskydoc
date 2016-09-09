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
{-# LANGUAGE LambdaCase #-}
{- |
Module      :  Text.Huskydoc.Pandoc
Copyright   :  © 2016 Albert Krewinkel
License     :  ISC

Maintainer  :  Albert Krewinkel <tarleb@zeitkraut.de>
Stability   :  experimental
Portability :  portable

Huskydoc element types
-}
module Text.Huskydoc.Pandoc
  ( convertDocument
  , convertBlocks
  , convertInlines
  ) where

import           Prelude hiding (foldr)
import           Data.Foldable (foldr)
import           Data.Monoid ((<>))
import           Data.Text (unpack)
import           Text.Huskydoc.Patterns
import qualified Text.Pandoc.Builder as Pandoc
import           Text.Pandoc.Definition (Pandoc)

-- | Convert a huskydoc AST into an pandoc AST
convertDocument :: Document -> Pandoc
convertDocument (Document _ bs) = Pandoc.doc . convertBlocks $ bs

-- | Convert huskydoc blocks into pandoc blocks
convertBlocks :: Blocks -> Pandoc.Blocks
convertBlocks = foldr ((<>) . convertBlockElement) mempty . fromBlocks

-- | Convert a single huskydoc block element into pandoc blocks
convertBlockElement :: BlockElement -> Pandoc.Blocks
convertBlockElement = \case
  (HorizontalRule)         -> Pandoc.horizontalRule
  (Paragraph inlns)        -> Pandoc.para (convertInlines inlns)
  (SectionTitle lvl inlns) -> Pandoc.header lvl (convertInlines inlns)
  (BulletList lst)         -> Pandoc.bulletList . concatMap convertListItem $ lst
  (Table rows)             -> let pandocRows = map convertRows rows
                                  headers = map (const mempty) (head pandocRows)
                              in Pandoc.simpleTable headers pandocRows
  _                        -> mempty

-- | Convert a list item to a list of pandoc blocks
convertListItem :: ListItem -> [Pandoc.Blocks]
convertListItem = map convertBlockElement . fromListItem

-- | Convert rows
convertRows :: TableRow -> [Pandoc.Blocks]
convertRows = map convertTableCell . fromTableRow

-- | Convert a table cell to pandoc blocks
convertTableCell :: TableCell -> Pandoc.Blocks
convertTableCell = convertBlocks . tableCellContent

-- | Convert huskydoc inlines into pandoc inlines
convertInlines :: Inlines -> Pandoc.Inlines
convertInlines = foldr ((<>) . convertInlineElement) mempty . fromInlines

convertInlineElement :: InlineElement -> Pandoc.Inlines
convertInlineElement = \case
  (Str txt)        -> Pandoc.str . unpack $ txt
  (Space)          -> Pandoc.space
  (HardBreak)      -> Pandoc.linebreak
  (SoftBreak)      -> Pandoc.softbreak
  (Emphasis inlns) -> Pandoc.emph   . convertInlines $ inlns
  (Strong   inlns) -> Pandoc.strong . convertInlines $ inlns
  (Superscript is) -> Pandoc.superscript . convertInlines $ is
  _                -> mempty
