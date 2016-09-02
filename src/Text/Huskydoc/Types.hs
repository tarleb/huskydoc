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
Module      :  Text.Huskydoc.Types
Copyright   :  © 2016 Albert Krewinkel
License     :  ISC

Maintainer  :  Albert Krewinkel <tarleb@zeitkraut.de>
Stability   :  experimental
Portability :  portable

Huskydoc element types
-}
module Text.Huskydoc.Types
  ( Attr (..)
  , Attributes (..)
  , Block (..)
  , BlockElement
  , Blocks (..)
  , Inline (..)
  , InlineElement
  , Inlines (..)
  , RichElement (..)
  , nullAttributes
  , plainElement
  , richElement
  , simpleNamedAttr
  , toAttributes
  ) where

import Data.Sequence
import Data.Text

-- | Element attributes
newtype Attributes = Attributes { fromAttributes :: [Attr] }
  deriving (Show, Eq, Ord)

data Attr =
    NamedAttr Text Text
  | PositionalAttr Text
  | OptionAttr Text
  deriving (Show, Eq, Ord)

toAttributes :: [Attr] -> Attributes
toAttributes = Attributes

nullAttributes :: Attributes
nullAttributes = toAttributes []

data RichElement a = RichElement
  { richElementAttributes :: Attributes
  , richElementContent    :: a
  } deriving (Eq, Ord, Show)

plainElement :: a -> RichElement a
plainElement x = RichElement nullAttributes x

richElement :: Attributes -> a -> RichElement a
richElement = RichElement

simpleNamedAttr :: Text -> Text -> Attr
simpleNamedAttr k v = NamedAttr k v

-- | Inline text types.
data Inline =
    Emphasis [InlineElement]
  | LineBreak
  | SoftBreak
  | Space
  | Str Text
  | Strong [InlineElement]
  deriving (Show, Eq, Ord)

type InlineElement = RichElement Inline

newtype Inlines = Inlines { fromInlines :: Seq InlineElement }
  deriving (Show, Eq, Ord)

-- | Block types
data Block =
    Paragraph Inlines
  deriving (Show, Eq, Ord)

type BlockElement = RichElement Block

newtype Blocks = Blocks { fromBlocks :: Seq BlockElement }
  deriving (Show, Eq, Ord)
