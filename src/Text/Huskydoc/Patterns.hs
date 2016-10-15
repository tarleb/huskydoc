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
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-|
Module      :  Text.Huskydoc.Patterns
Copyright   :  © 2016 Albert Krewinkel
License     :  ISC

Maintainer  :  Albert Krewinkel <tarleb@zeitkraut.de>
Stability   :  experimental
Portability :  portable

Patterns for Huskydoc elements
-}
module Text.Huskydoc.Patterns
  ( module Text.Huskydoc.Types
  -- document
  , emptyMeta
  -- Attributes
  , pattern EmptyAttributes
  -- Plain blocks
  , pattern BulletList
  , pattern HorizontalRule
  , pattern OrderedList
  , pattern Paragraph
  , pattern SectionTitle
  , pattern Table
  -- Rich blocks
  , pattern RichBulletList
  , pattern RichHorizontalRule
  , pattern RichOrderedList
  , pattern RichParagraph
  , pattern RichSectionTitle
  , pattern RichSource
  , pattern RichTable
  -- Inlines
  , pattern Emphasis
  , pattern HardBreak
  , pattern Image
  , pattern Link
  , pattern Monospaced
  , pattern SoftBreak
  , pattern Space
  , pattern Str
  , pattern Strong
  , pattern Subscript
  , pattern Superscript
  , stripInlines
  -- Rich inlines
  , pattern RichEmphasis
  , pattern RichHardBreak
  , pattern RichImage
  , pattern RichLink
  , pattern RichMonospaced
  , pattern RichSoftBreak
  , pattern RichSpace
  , pattern RichStr
  , pattern RichStrong
  , pattern RichSubscript
  , pattern RichSuperscript
  ) where

import Data.Sequence ( Seq, ViewL (..), ViewR (..), viewl, viewr )
import Data.Text ( Text )
import Text.Huskydoc.Types ( Attributes (..), Attr (..)
                           , RichElement (..)
                           , Document (..), Metadata (..)
                           , BlockElement, Blocks (..)
                           , InlineElement, Inlines (..)
                           , ListItem (..), SourceLine (..)
                           , TableRow (..), TableCell (..)
                           , plainElement , nullAttributes
                           )
import qualified Text.Huskydoc.Types as Internal

--
-- Document
--

-- | Empty metadata
emptyMeta :: Metadata
emptyMeta = mempty


--
-- Misc
--

-- | Empty attributes
pattern EmptyAttributes :: Attributes
pattern EmptyAttributes = Attributes []

--
-- Inline elements
--

-- | A simple element of emphasized text.
pattern Emphasis :: Inlines -> InlineElement
pattern Emphasis inlns <- RichElement _ (Internal.Emphasis inlns)
  where Emphasis inlns = plainElement . Internal.Emphasis $ inlns

-- | Hard linebreak element
pattern HardBreak :: InlineElement
pattern HardBreak <- RichElement _ Internal.LineBreak
  where HardBreak = plainElement Internal.LineBreak

-- | Image element with image source
pattern Image :: Text -> InlineElement
pattern Image src <- RichElement _ (Internal.Image src)
  where Image src = plainElement $ Internal.Image src

-- | Link element with target location and descriptions
pattern Link :: Text -> Inlines -> InlineElement
pattern Link ref desc <- RichElement _ (Internal.Link ref desc)
  where Link ref desc = plainElement $ Internal.Link ref desc

-- | Monospaced element with target location and descriptions
pattern Monospaced :: Inlines -> InlineElement
pattern Monospaced inlns <- RichElement _ (Internal.Monospaced inlns)
  where Monospaced inlns = plainElement $ Internal.Monospaced inlns

-- | Soft linebreak element
pattern SoftBreak :: InlineElement
pattern SoftBreak <- RichElement _ Internal.SoftBreak
  where SoftBreak = plainElement Internal.SoftBreak

-- | Sspace element
pattern Space :: InlineElement
pattern Space <- RichElement _ Internal.Space
  where Space = plainElement Internal.Space

-- | Simple text element
pattern Str :: Text -> InlineElement
pattern Str txt <- RichElement _ (Internal.Str txt)
  where Str = plainElement . Internal.Str

-- | Strong element
pattern Strong :: Inlines -> InlineElement
pattern Strong inlns <- RichElement _ (Internal.Strong inlns)
  where Strong = plainElement . Internal.Strong

-- | Subscript element
pattern Subscript :: Inlines -> InlineElement
pattern Subscript inlns <- RichElement _ (Internal.Subscript inlns)
  where Subscript = plainElement . Internal.Subscript

-- | Superscript element
pattern Superscript :: Inlines -> InlineElement
pattern Superscript inlns <- RichElement _ (Internal.Superscript inlns)
  where Superscript = plainElement . Internal.Superscript

-- | Strip leading and trainling whitespace from inlines
stripInlines :: Inlines -> Inlines
stripInlines = Inlines . trimInlinesLeft . trimInlinesRight . fromInlines
  where
    trimInlinesLeft :: Seq InlineElement -> Seq InlineElement
    trimInlinesLeft inlns = case viewl inlns of
      Space :< is     -> trimInlinesLeft is
      HardBreak :< is -> trimInlinesLeft is
      SoftBreak :< is -> trimInlinesLeft is
      _               -> inlns

    trimInlinesRight :: Seq InlineElement -> Seq InlineElement
    trimInlinesRight inlns = case viewr inlns of
      is :> Space     -> trimInlinesRight is
      is :> HardBreak -> trimInlinesRight is
      is :> SoftBreak -> trimInlinesRight is
      _               -> inlns


--
-- Inline elements with attributes ("Rich" elements)
--

-- | An element for emphasized text with attributes
pattern RichEmphasis :: Attributes -> Inlines -> InlineElement
pattern RichEmphasis attr inlns = RichElement attr (Internal.Emphasis inlns)

-- | Hard linebreak element with attributes
pattern RichHardBreak :: Attributes -> InlineElement
pattern RichHardBreak attrs = RichElement attrs Internal.LineBreak

-- | Image element with attributes
pattern RichImage :: Attributes -> Text -> InlineElement
pattern RichImage attrs src = RichElement attrs (Internal.Image src)

-- | Link element with target location, descriptions and attributes
pattern RichLink :: Attributes -> Text -> Inlines -> InlineElement
pattern RichLink attr ref desc <- RichElement attr (Internal.Link ref desc)

-- | Monospaced text with attributes
pattern RichMonospaced :: Attributes -> Inlines -> InlineElement
pattern RichMonospaced attr inlns = RichElement attr (Internal.Monospaced inlns)

-- | Soft linebreak element with attributes
pattern RichSoftBreak :: Attributes -> InlineElement
pattern RichSoftBreak attrs = RichElement attrs Internal.SoftBreak

-- | Space with attributes
pattern RichSpace :: Attributes -> InlineElement
pattern RichSpace attr = RichElement attr Internal.Space

-- | Text element with attributes
pattern RichStr :: Attributes -> Text -> InlineElement
pattern RichStr attr txt = RichElement attr (Internal.Str txt)

-- | Strong text with attributes
pattern RichStrong :: Attributes -> Inlines -> InlineElement
pattern RichStrong attr inlns = RichElement attr (Internal.Strong inlns)

-- | Subscript text with attributes
pattern RichSubscript :: Attributes -> Inlines -> InlineElement
pattern RichSubscript attr inlns = RichElement attr (Internal.Subscript inlns)

-- | Superscript text with attributes
pattern RichSuperscript :: Attributes -> Inlines -> InlineElement
pattern RichSuperscript attr inlns = RichElement attr (Internal.Superscript inlns)

--
-- Plain block elements
--

-- | Bullet list
pattern BulletList :: [ListItem] -> BlockElement
pattern BulletList es <- RichElement _ (Internal.BulletList es)
  where BulletList = plainElement . Internal.BulletList

-- | Horizontal rule element
pattern HorizontalRule :: BlockElement
pattern HorizontalRule <- RichElement _ Internal.HorizontalRule
  where HorizontalRule = plainElement Internal.HorizontalRule

-- | Bullet list
pattern OrderedList :: [ListItem] -> BlockElement
pattern OrderedList es <- RichElement _ (Internal.OrderedList es)
  where OrderedList = plainElement . Internal.OrderedList

-- | Paragraph element
pattern Paragraph :: Inlines -> BlockElement
pattern Paragraph blks <- RichElement _ (Internal.Paragraph blks)
  where Paragraph = plainElement . Internal.Paragraph

-- | Section title element
pattern SectionTitle :: Int -> Inlines -> BlockElement
pattern SectionTitle lvl inlns <- RichElement _ (Internal.SectionTitle lvl inlns)
  where SectionTitle lvl inlns = plainElement (Internal.SectionTitle lvl inlns)

pattern Table :: [TableRow] -> BlockElement
pattern Table rows <- RichElement _ (Internal.Table rows)
  where Table rows = plainElement (Internal.Table rows)

--
-- Block elements with attributes ("Rich" block elements)
--

-- | Bullet (i.e. unordered) list with attributes
pattern RichBulletList :: Attributes -> [ListItem] -> BlockElement
pattern RichBulletList attr es = RichElement attr (Internal.BulletList es)

-- | Horizontal rule element with attributes
pattern RichHorizontalRule :: Attributes -> BlockElement
pattern RichHorizontalRule attr = RichElement attr Internal.HorizontalRule

-- | Bullet (i.e. unordered) list with attributes
pattern RichOrderedList :: Attributes -> [ListItem] -> BlockElement
pattern RichOrderedList attr es = RichElement attr (Internal.OrderedList es)

-- | Paragraph element with attributes
pattern RichParagraph :: Attributes -> Inlines -> BlockElement
pattern RichParagraph attr blks = RichElement attr (Internal.Paragraph blks)

-- | Section title element with attributes
pattern RichSectionTitle :: Attributes -> Int -> Inlines -> BlockElement
pattern RichSectionTitle attr lvl blks = RichElement attr (Internal.SectionTitle lvl blks)

-- | Source block element with attributes
pattern RichSource :: Attributes -> [SourceLine] -> BlockElement
pattern RichSource attr srcLines = RichElement attr (Internal.Source srcLines)

-- | Table with attributes
pattern RichTable :: Attributes -> [TableRow] -> BlockElement
pattern RichTable attr rows = RichElement attr (Internal.Table rows)
