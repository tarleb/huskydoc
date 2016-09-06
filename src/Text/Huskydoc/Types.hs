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
  , Document (..)
  , Inline (..)
  , InlineElement
  , Inlines (..)
  , ListItem (..)
  , MetaData (..)
  , RichElement (..)
  , nullAttributes
  , plainElement
  , richElement
  , simpleNamedAttr
  , toAttributes
  , toBlocksList
  , toInlinesList
  ) where

import Data.Foldable (toList)
import Data.Monoid ( (<>) )
import Data.Sequence
import Data.Text

-- | The full document
data Document = Document MetaData Blocks deriving (Show, Eq)

-- | Meta data of a document.
data MetaData = MetaData
    { metaDataTitle :: Inlines
    } deriving (Show, Eq)

-- | Element attributes
newtype Attributes = Attributes { fromAttributes :: [Attr] }
  deriving (Show, Eq, Ord)

instance Monoid Attributes where
  (Attributes a) `mappend` (Attributes b) = Attributes (a <> b)
  mempty = nullAttributes

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
    Emphasis Inlines
  | LineBreak
  | Link Text Inlines
  | SoftBreak
  | Space
  | Str Text
  | Strong Inlines
  deriving (Show, Eq, Ord)

type InlineElement = RichElement Inline

newtype Inlines = Inlines { fromInlines :: Seq InlineElement }
  deriving (Show, Eq, Ord)

toInlinesList :: Inlines -> [InlineElement]
toInlinesList = toList . fromInlines

-- | Block types
data Block =
    HorizontalRule
  | Paragraph Inlines
  | SectionTitle Int Inlines
  | BulletList [ListItem]
  deriving (Show, Eq, Ord)

-- | A single block element, the main building blocks of documents
type BlockElement = RichElement Block

-- | Elements of block-lists
newtype ListItem = ListItem { fromListItem :: [BlockElement] }
  deriving (Show, Eq, Ord)

-- | Sequence of block elements, the basic components of documents
newtype Blocks = Blocks { fromBlocks :: Seq BlockElement }
  deriving (Show, Eq, Ord)

toBlocksList :: Blocks -> [BlockElement]
toBlocksList = toList . fromBlocks
