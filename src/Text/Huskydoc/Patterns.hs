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
{- |
Module      :  Text.Huskydoc.Patterns
Copyright   :  © 2016 Albert Krewinkel
License     :  ISC

Maintainer  :  Albert Krewinkel <tarleb@zeitkraut.de>
Stability   :  experimental
Portability :  portable

Huskydoc element patterns
-}
module Text.Huskydoc.Patterns
  ( module Text.Huskydoc.Types
  , pattern RichHorizontalRule
  , pattern RichParagraph
  , pattern RichSectionTitle
  -- Inlines
  , pattern RichEmphasis
  , pattern RichHardBreak
  , pattern RichSoftBreak
  , pattern RichSpace
  , pattern RichStr
  , pattern RichStrong
  ) where

import           Text.Huskydoc.Types

pattern RichHorizontalRule attr = RichElement attr HorizontalRule
pattern RichParagraph attr blks = RichElement attr (Paragraph blks)
pattern RichSectionTitle attr lvl blks = RichElement attr (SectionTitle lvl blks)

--
-- Inlines
--
pattern RichEmphasis attr inlns  <- RichElement attr (Emphasis inlns)
pattern RichHardBreak <- RichElement _ LineBreak
pattern RichSoftBreak <- RichElement _ SoftBreak
pattern RichSpace     <- RichElement _ Space
pattern RichStr attr txt = RichElement attr (Str txt)
pattern RichStrong attr inlns    <- RichElement attr (Strong inlns)
