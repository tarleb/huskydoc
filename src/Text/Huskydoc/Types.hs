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
{-# LANGUAGE TypeFamilies #-}
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
  , Metadata (..)
  , RichElement (..)
  , SourceLine (..)
  , TableRow (..)
  , TableCell (..)
  , nullAttributes
  , plainElement
  , richElement
  , toAttributes
  ) where

import           Data.Default ( Default(..) )
import           Data.Foldable as Foldable
import           Data.Monoid ( (<>) )
import           Data.Sequence ( Seq )
import qualified Data.Sequence as Seq
import           Data.Text
import           GHC.Exts ( IsList (..) )

-- | The full document
data Document = Document Metadata Blocks deriving (Show, Eq)

-- | Meta data of a document.
data Metadata = Metadata
  { metadataTitle    :: Inlines
  , metadataAuthor   :: Maybe Text
  , metadataEmail    :: Maybe Text
  , metadataRevision :: Maybe Text
  } deriving (Show, Eq)

instance Default Metadata where
  def = Metadata
    { metadataTitle    = []
    , metadataAuthor   = Nothing
    , metadataEmail    = Nothing
    , metadataRevision = Nothing
    }

instance Monoid Metadata where
  mempty = Metadata mempty Nothing Nothing Nothing
  a `mappend` b =
    Metadata { metadataTitle    = metadataTitle a <> metadataTitle b
             , metadataAuthor   = metadataAuthor a <> metadataAuthor b
             , metadataEmail    = metadataEmail a <> metadataEmail b
             , metadataRevision = metadataRevision a <> metadataRevision b
             }

-- | Element attributes
newtype Attributes = Attributes { fromAttributes :: [Attr] }
  deriving (Show, Eq, Ord)

instance Monoid Attributes where
  (Attributes a) `mappend` (Attributes b) = Attributes (a <> b)
  mempty = nullAttributes

data Attr = Attr
  { attrKey :: Text
  , attrValue :: Text
  }
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

-- | Inline text types.
data Inline =
    Emphasis Inlines
  | Image Text
  | LineBreak
  | Link Text Inlines
  | Monospaced Inlines
  | SoftBreak
  | Space
  | Str Text
  | Strong Inlines
  | Subscript Inlines
  | Superscript Inlines
  deriving (Show, Eq, Ord)

type InlineElement = RichElement Inline

-- | Inlines are the basic components of larger text elements.
newtype Inlines = Inlines { fromInlines :: Seq InlineElement }
  deriving (Show, Eq, Ord)

instance IsList Inlines where
  type Item Inlines = InlineElement
  fromList = Inlines . Seq.fromList
  toList   = Foldable.toList . fromInlines

instance Monoid Inlines where
  mempty = Inlines mempty
  (Inlines a) `mappend` (Inlines b) = Inlines (a <> b)

-- | Block types
data Block =
    BulletList [ListItem]
  | HorizontalRule
  | OrderedList [ListItem]
  | Paragraph Inlines
  | SectionTitle Int Inlines
  | Source [SourceLine]
  | Table [TableRow]
  deriving (Show, Eq, Ord)

-- | Source block line
newtype SourceLine = SourceLine { fromSourceLine :: Text }
  deriving (Eq, Ord, Show)

-- | Table row
newtype TableRow = TableRow { fromTableRow :: [TableCell] }
  deriving (Eq, Ord, Show)

instance IsList TableRow where
  type Item TableRow = TableCell
  fromList = TableRow
  toList   = fromTableRow

-- | A table cell with span info and content
data TableCell = TableCell
  { tableCellContent :: Blocks}
  deriving (Eq, Ord, Show)

-- | A single block element, the main building blocks of documents
type BlockElement = RichElement Block

-- | Elements of block-lists
newtype ListItem = ListItem { fromListItem :: Blocks }
  deriving (Show, Eq, Ord)

-- | Sequence of block elements, the basic components of documents
newtype Blocks = Blocks { fromBlocks :: Seq BlockElement }
  deriving (Show, Eq, Ord)

instance IsList Blocks where
  type Item Blocks = BlockElement
  fromList = Blocks . Seq.fromList
  toList   = Foldable.toList . fromBlocks
