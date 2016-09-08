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
  , metaData
  , emptyMeta
  -- Attributes
  , pattern EmptyAttributes
  -- Plain blocks
  , pattern HorizontalRule
  , pattern BulletList
  , pattern Paragraph
  , pattern SectionTitle
  , pattern Table
  -- Rich blocks
  , pattern RichHorizontalRule
  , pattern RichBulletList
  , pattern RichParagraph
  , pattern RichSectionTitle
  , pattern RichTable
  -- Inlines
  , pattern Emphasis
  , pattern HardBreak
  , pattern Link
  , pattern SoftBreak
  , pattern Space
  , pattern Str
  , pattern Strong
  -- Rich inlines
  , pattern RichEmphasis
  , pattern RichHardBreak
  , pattern RichLink
  , pattern RichSoftBreak
  , pattern RichSpace
  , pattern RichStr
  , pattern RichStrong
  , sectionTitleWith
  , paragraphWith
  ) where

import           Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import           Text.Huskydoc.Types ( Attributes (..), RichElement (..)
                                     , Document (..), MetaData (..)
                                     , BlockElement, Blocks (..)
                                     , InlineElement, Inlines (..)
                                     , ListItem (..)
                                     , TableRow (..), TableCell (..)
                                     , plainElement , nullAttributes
                                     )
import qualified Text.Huskydoc.Types as Internal

--
-- Document
--

-- | Empty metadata
emptyMeta :: MetaData
emptyMeta = MetaData emptyInlines

-- | Create a new metadata element
metaData :: Inlines -> MetaData
metaData metaDataTitle = MetaData {..}


--
-- wrappers
--
-- | Empty inlines
emptyInlines :: Inlines
emptyInlines = Internal.Inlines Seq.empty


--
-- Misc
--

-- | Empty attributes
pattern EmptyAttributes = Attributes []

--
-- Inline elements
--

-- | A simple element of emphasized text.
-- Emphasis :: Inlines -> InlineElement
pattern Emphasis inlns <- RichElement _ (Internal.Emphasis inlns)
  where Emphasis inlns = plainElement . Internal.Emphasis $ inlns

-- | Hard linebreak element
pattern HardBreak <- RichElement _ Internal.LineBreak
  where HardBreak = plainElement Internal.LineBreak

-- | Link element with target location and descriptions
pattern Link ref desc <- RichElement _ (Internal.Link ref desc)
  where Link ref desc = plainElement $ Internal.Link ref desc

-- | Soft linebreak element
pattern SoftBreak <- RichElement _ Internal.SoftBreak
  where SoftBreak = plainElement Internal.SoftBreak

-- | Sspace element
pattern Space <- RichElement _ Internal.Space
  where Space = plainElement Internal.Space

-- | Simple text element
pattern Str txt <- RichElement _ (Internal.Str txt)
  where Str = plainElement . Internal.Str

-- | Plain elements
pattern Strong inlns <- RichElement _ (Internal.Strong inlns)
  where Strong = plainElement . Internal.Strong


--
-- Inline elements with attributes ("Rich" elements)
--

-- | An element for emphasized text with attributes
-- RichEmphasis :: Inlines -> InlineElement
pattern RichEmphasis attr inlns = RichElement attr (Internal.Emphasis inlns)

-- | Hard linebreak element with attributes
pattern RichHardBreak attrs = RichElement attrs Internal.LineBreak

-- | Link element with target location, descriptions and attributes
pattern RichLink attr ref desc <- RichElement attr (Internal.Link ref desc)

-- | Soft linebreak element with attributes
pattern RichSoftBreak attrs = RichElement attrs Internal.SoftBreak

-- | Space with attributes
pattern RichSpace attr = RichElement attr Internal.Space

-- | Text element with attributes
pattern RichStr attr txt = RichElement attr (Internal.Str txt)

-- | Strong text with attributes
pattern RichStrong attr inlns = RichElement attr (Internal.Strong inlns)

--
-- Plain block elements
--
-- | Horizontal rule element
pattern HorizontalRule <- RichElement _ Internal.HorizontalRule
  where HorizontalRule = plainElement Internal.HorizontalRule

-- | Bullet list
pattern BulletList es <- RichElement _ (Internal.BulletList es)
  where BulletList = plainElement . Internal.BulletList

-- | Paragraph element
pattern Paragraph blks <- RichElement _ (Internal.Paragraph blks)
  where Paragraph = plainElement . Internal.Paragraph

-- | Section title element
pattern SectionTitle lvl inlns <- RichElement _ (Internal.SectionTitle lvl inlns)
  where SectionTitle lvl inlns = plainElement (Internal.SectionTitle lvl inlns)

pattern Table rows <- RichElement _ (Internal.Table rows)
  where Table rows = plainElement (Internal.Table rows)

--
-- Block elements with attributes ("Rich" block elements)
--

-- | Horizontal rule element with attributes
pattern RichHorizontalRule attr = RichElement attr Internal.HorizontalRule

-- | Bullet (i.e. unordered) list with attributes
pattern RichBulletList attr es = RichElement attr (Internal.BulletList es)

-- | Paragraph element with attributes
pattern RichParagraph attr blks = RichElement attr (Internal.Paragraph blks)

-- | Section title element with attributes
pattern RichSectionTitle attr lvl blks = RichElement attr (Internal.SectionTitle lvl blks)

-- | Table with attributes
pattern RichTable attr rows = RichElement attr (Internal.Table rows)


paragraphWith :: Inlines -> Maybe Attributes -> BlockElement
paragraphWith inlns a = fromMaybe nullAttributes a `RichParagraph` inlns

sectionTitleWith :: Int -> Inlines -> Maybe Attributes -> BlockElement
sectionTitleWith lvl inlns a = (maybe SectionTitle RichSectionTitle a) lvl $ inlns
