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
{-# LANGUAGE OverloadedStrings #-}
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

import           Prelude hiding ( foldr )
import           Data.Foldable ( foldl', foldr )
import           Data.Monoid ((<>))
import           Data.Text ( Text, unpack )
import           GHC.Exts ( IsList (..) )
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
  (BulletList lst)         -> Pandoc.bulletList . map convertListItem $ lst
  (HorizontalRule)         -> Pandoc.horizontalRule
  (OrderedList lst)        -> Pandoc.orderedList . map convertListItem $ lst
  (Paragraph inlns)        -> Pandoc.para (convertInlines inlns)
  (SectionTitle lvl inlns) -> Pandoc.header lvl (convertInlines inlns)
  (Table rows)             -> let pandocRows = map convertRows rows
                                  headers = map (const mempty) (head pandocRows)
                              in Pandoc.simpleTable headers pandocRows
  _                        -> mempty

-- | Convert a list item to a list of pandoc blocks
convertListItem :: ListItem -> Pandoc.Blocks
convertListItem =
  mconcat . map convertBlockElement . toList . fromBlocks . fromListItem

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
  (Emphasis inlns)    -> Pandoc.emph   . convertInlines $ inlns
  (HardBreak)         -> Pandoc.linebreak
  (Image src)         -> Pandoc.image (unpack src) mempty mempty
  (Monospaced inlns)  -> Pandoc.code . unpack . extractText $ inlns
  (SoftBreak)         -> Pandoc.softbreak
  (Space)             -> Pandoc.space
  (Str txt)           -> Pandoc.str . unpack $ txt
  (Strong   inlns)    -> Pandoc.strong . convertInlines $ inlns
  (Subscript is)      -> Pandoc.subscript . convertInlines $ is
  (Superscript is)    -> Pandoc.superscript . convertInlines $ is
  _                   -> mempty -- suppress GHC warnings

-- | Convert inlines to raw text
extractText :: Inlines -> Text
extractText = foldl' step mempty . fromInlines
  where
    step :: Text -> InlineElement -> Text -- -> Text
    step acc inlns = mappend acc $
      case inlns of
        (Emphasis inlns')    -> extractText inlns'
        (HardBreak)          -> "\n"
        (Link _ desc)        -> extractText desc
        (Monospaced inlns')  -> extractText inlns'
        (SoftBreak)          -> " "
        (Space)              -> " "
        (Str txt)            -> txt
        (Strong inlns')      -> extractText inlns'
        (Subscript inlns')   -> extractText inlns'
        (Superscript inlns') -> extractText inlns'
        _                    -> mempty
