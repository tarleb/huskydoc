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
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      :  Text.Huskydoc.TableSpec
Copyright   :  © 2016 Albert Krewinkel
License     :  ISC

Maintainer  :  Albert Krewinkel <tarleb@zeitkraut.de>
Stability   :  experimental
Portability :  portable

Tests for table component parsers.
-}
module Text.Huskydoc.TableSpec
  ( main
  , spec
  ) where

import Text.Huskydoc.Parsing ( parseDef, char )
import Text.Huskydoc.Patterns
import Text.Huskydoc.Table

import Test.Hspec
import Test.Hspec.Megaparsec

-- | Run this spec.
main :: IO ()
main = hspec spec

-- | Specifications for Attributes parsing functions.
spec :: Spec
spec = do
  describe "tableCell parser" $ do
    it "parses a single table cell, ended by eol or another cell" $ do
      parseDef (tableCell *> char '|') `shouldSucceedOn `"hello |"
    it "parses a single table cell, ended by eol or another cell" $ do
      parseDef (tableCell *> char '\n') `shouldSucceedOn `"hello \n"

  describe "tableRow parser" $ do
    it "parses a table row, ended by eol" $ do
      parseDef tableRow `shouldSucceedOn `"| hello | world\n"
      parseDef tableRow "| hello | world \n" `shouldParse `
        (TableRow [ TableCell [Paragraph [Str "hello"]]
                  , TableCell [Paragraph [Str "world"]]])

  describe "table parser" $ do
    it "parses a simple table" $ do
      parseDef (attribless table) `shouldSucceedOn` "|===\n|moin | moin\n|===\n"

-- | Helper function for attribute-less blocks
attribless :: Applicative f => f (Attributes -> a) -> f a
attribless p = p <*> pure mempty
