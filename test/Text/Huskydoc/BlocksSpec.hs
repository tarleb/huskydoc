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

import Text.Huskydoc.Blocks
import Text.Huskydoc.Parsing ( parseDef, char )
import Text.Huskydoc.Patterns

import Test.Hspec
import Test.Hspec.Megaparsec

-- | Run this spec.
main :: IO ()
main = hspec spec

-- | Specifications for Attributes parsing functions.
spec :: Spec
spec = do
  describe "horizontalRule" $ do
    it "parses a simple horizontal rule" $ do
      parseDef (attribless horizontalRule) "- - -\n" `shouldParse` HorizontalRule
    it "fails if the rule is followed by non-whitespace chars" $ do
      parseDef (attribless horizontalRule) `shouldFailOn` "- - - -\n"
      parseDef (attribless horizontalRule) `shouldFailOn` "- - --\n"
      parseDef (attribless horizontalRule) `shouldFailOn` "- - - %\n"
      parseDef (attribless horizontalRule) `shouldFailOn` "- - - a\n"

  describe "paragraph" $ do
    it "parses a simple paragraph" $ do
      parseDef (attribless paragraph) "Single line paragraph" `shouldParse`
        (Paragraph [Str "Single", Space, Str "line", Space, Str "paragraph"])
    it "should fail on empty input" $
      parseDef (attribless paragraph) `shouldFailOn` ""
    it "should fail on whitespace-only lines" $
      parseDef (attribless paragraph) `shouldFailOn` "   \n\n"

  describe "sectionTitle" $ do
    it "parses a section title" $ do
      parseDef (attribless sectionTitle) "== Level1" `shouldParse`
        (SectionTitle 1 [Str "Level1"])
    it "should only consume inline text in the current line" $ do
      parseDef (attribless sectionTitle) "== Level1\nNext line" `shouldParse`
        (SectionTitle 1 [Str "Level1"])
    it "should parse underlined titles" $ do
      parseDef (attribless sectionTitle) "Level0\n======\n" `shouldParse`
        (SectionTitle 0 [Str "Level0"])
      parseDef (attribless sectionTitle) "Level1\n------\n" `shouldParse`
        (SectionTitle 1 [Str "Level1"])
      parseDef (attribless sectionTitle) "Level2\n~~~~~~\n" `shouldParse`
        (SectionTitle 2 [Str "Level2"])
      parseDef (attribless sectionTitle) "Level3\n^^^^^^\n" `shouldParse`
        (SectionTitle 3 [Str "Level3"])
    it "should succeed when combined with block attributes parser" $ do
      parseDef (withBlockAttributes sectionTitle) `shouldSucceedOn`
        "[[cheat-sheet]]\nAsciiDoc Mini Cheat Sheet\n~~~~~~~~~~~~~~~~~~~~~~~~~\n"
    it "should fail if the underline is far too short or too long" $ do
      parseDef (attribless sectionTitle) `shouldFailOn `"Level0\n====\n"
      parseDef (attribless sectionTitle) `shouldFailOn `"Level0\n========\n"

  describe "listItem parser" $ do
    it "parses a bullet list item" $ do
      parseDef (listItem "**") "** one" `shouldParse`
        ListItem [Paragraph [Str "one"]]
    it "parses a bullet list item with continuation" $ do
      parseDef (listItem "*") "* first:\n+\nnext one\n\n" `shouldParse`
        ListItem [ Paragraph [Str "first:"]
                 , Paragraph [Str "next", Space, Str "one"]]

  describe "bulletList parser" $ do
    it "parses a single element bullet list" $ do
      parseDef (attribless bulletList) "* one\n" `shouldParse`
        BulletList [ ListItem [Paragraph [Str "one", SoftBreak]]]
    it "parses a bullet list" $ do
      parseDef (attribless bulletList) "** one\n** two\n** three\n" `shouldParse`
        BulletList [ ListItem [Paragraph [Str "one"]]
                   , ListItem [Paragraph [Str "two"]]
                   , ListItem [Paragraph [Str "three", SoftBreak]]
                   ]
    it "parses a nested bullet list" $ do
      parseDef (attribless bulletList) "** one\n* 1a\n** two\n" `shouldParse`
        BulletList [ ListItem [ Paragraph [Str "one"]
                              , BulletList [ ListItem [Paragraph [Str "1a"]]]
                              ]
                   , ListItem [Paragraph [Str "two", SoftBreak]]
                   ]

    it "parses a bullet list with continuations" $ do
      parseDef (attribless bulletList) "* Image:\n+\nimage:sunset.jpg[]\n\n* End" `shouldParse`
        BulletList [ ListItem [ Paragraph [Str "Image:"]
                              , Paragraph [Image "sunset.jpg"]
                              ]
                   , ListItem [ Paragraph [Str "End"] ]
                   ]

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
