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

{-# LANGUAGE OverloadedStrings #-}
{-|
Module      :  Text.Huskydoc.BlocksSpec
Copyright   :  © 2016 Albert Krewinkel
License     :  ISC

Maintainer  :  Albert Krewinkel <tarleb@zeitkraut.de>
Stability   :  experimental
Portability :  portable

Tests for block parsers.
-}
module Text.Huskydoc.BlocksSpec
  ( main
  , spec
  ) where

import           Text.Huskydoc.Blocks
import qualified Text.Huskydoc.Builders as B
import           Text.Huskydoc.Parsing ( parseDef )

import           Test.Hspec
import           Test.Hspec.Megaparsec

-- | Run this spec.
main :: IO ()
main = hspec spec

-- | Specifications for Attributes parsing functions.
spec :: Spec
spec = do
  describe "horizontalRule" $ do
    it "parses a simple horizontal rule" $ do
      parseDef horizontalRule "- - -\n" `shouldParse` B.horizontalRule
    it "fails if the rule is followed by non-whitespace chars" $ do
      parseDef horizontalRule `shouldFailOn` "- - - -\n"
      parseDef horizontalRule `shouldFailOn` "- - --\n"
      parseDef horizontalRule `shouldFailOn` "- - - %\n"
      parseDef horizontalRule `shouldFailOn` "- - - a\n"

  describe "paragraph" $ do
    it "parses a simple paragraph" $ do
      parseDef paragraph "Single line paragraph" `shouldParse`
        (B.paragraph $ B.toInlines [B.str "Single", B.space, B.str "line", B.space, B.str "paragraph"])
    it "should fail on empty input" $
      parseDef paragraph `shouldFailOn` ""
    it "should fail on whitespace-only lines" $
      parseDef paragraph `shouldFailOn` "   \n\n"

  describe "sectionTitle" $ do
    it "parses a section title" $ do
      parseDef sectionTitle "== Level1" `shouldParse`
        (B.sectionTitle 1 (B.toInlines [B.str "Level1"]))
    it "should only consume inline text in the current line" $ do
      parseDef sectionTitle "== Level1\nNext line" `shouldParse`
        (B.sectionTitle 1 (B.toInlines [B.str "Level1"]))
    it "should parse underlined titles" $ do
      parseDef sectionTitle "Level0\n======\n" `shouldParse`
        (B.sectionTitle 0 (B.toInlines [B.str "Level0"]))
      parseDef sectionTitle "Level1\n------\n" `shouldParse`
        (B.sectionTitle 1 (B.toInlines [B.str "Level1"]))
      parseDef sectionTitle "Level2\n~~~~~~\n" `shouldParse`
        (B.sectionTitle 2 (B.toInlines [B.str "Level2"]))
      parseDef sectionTitle "Level3\n^^^^^^\n" `shouldParse`
        (B.sectionTitle 3 (B.toInlines [B.str "Level3"]))
    it "should fail if the underline is far too short or too long" $ do
      parseDef sectionTitle `shouldFailOn `"Level0\n====\n"
      parseDef sectionTitle `shouldFailOn `"Level0\n========\n"
