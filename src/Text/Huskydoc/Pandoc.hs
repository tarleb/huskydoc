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
import qualified Text.Huskydoc.Builders as B
import           Text.Huskydoc.Patterns
import           Text.Huskydoc.Types
import qualified Text.Pandoc.Builder as Pandoc
import           Text.Pandoc.Definition (Pandoc)

-- | Convert a huskydoc AST into an pandoc AST
convertDocument :: Document -> Pandoc
convertDocument (Document _ bs) = Pandoc.doc . convertBlocks $ bs

-- | Convert huskydoc blocks into pandoc blocks
convertBlocks :: Blocks -> Pandoc.Blocks
convertBlocks = foldr ((<>) . convertBlockElement) mempty . fromBlocks

convertBlockElement :: BlockElement -> Pandoc.Blocks
convertBlockElement = \case
  (RichParagraph _ inlns) -> Pandoc.para (convertInlines inlns)
  (RichSectionTitle _ lvl inlns) -> Pandoc.header lvl (convertInlines inlns)
  _ -> mempty

-- | Convert huskydoc inlines into pandoc inlines
convertInlines :: Inlines -> Pandoc.Inlines
convertInlines = foldr ((<>) . convertInlineElement) mempty . fromInlines

convertInlineElement :: InlineElement -> Pandoc.Inlines
convertInlineElement = \case
  (RichSpace)               -> Pandoc.space
  (RichStr _ txt)           -> Pandoc.str . unpack $ txt
  (RichHardBreak)           -> Pandoc.linebreak
  (RichSoftBreak)           -> Pandoc.softbreak
  (RichEmphasis _ inlns)    -> Pandoc.emph   . convertInlines . B.toInlines $ inlns
  (RichStrong   _ inlns)    -> Pandoc.strong . convertInlines . B.toInlines $ inlns
  _                         -> mempty
