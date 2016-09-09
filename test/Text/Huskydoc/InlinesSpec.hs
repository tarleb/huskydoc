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
Module      :  Text.Huskydoc.InlinesSpec
Copyright   :  © 2016 Albert Krewinkel
License     :  ISC

Maintainer  :  Albert Krewinkel <tarleb@zeitkraut.de>
Stability   :  experimental
Portability :  portable

Tests for the inlines parsers.
-}
module Text.Huskydoc.InlinesSpec
  ( main
  , spec
  ) where

import Data.Text
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Huskydoc.Inlines
import Text.Huskydoc.Parsing ( parseDef, (<|>) )
import Text.Huskydoc.Patterns

-- | Run this spec.
main :: IO ()
main = hspec spec

-- | Specifications for Inlines parsing functions.
spec :: Spec
spec = do
  describe "soft linebreaks parser" $ do
    it "parses crlf" $ do
      parseDef softbreak "\r\n" `shouldParse` SoftBreak
    it "parses linefeed" $ do
      parseDef softbreak "\n" `shouldParse` SoftBreak
    it "allows spaces before linefeed" $ do
      parseDef softbreak "   \n" `shouldParse` SoftBreak
    it "should fail if the next line is blank" $ do
      parseDef softbreak `shouldFailOn` "\n\n"
    it "should fail if the next line looks like a list item" $ do
      parseDef softbreak `shouldFailOn` "\n* item\n"

  describe "hard linebreaks parser" $ do
    it "parses spaces, continuation char `+`, and crlf" $ do
      parseDef hardbreak " +\r\n" `shouldParse` HardBreak
    it "parses spaces,  continuation char `+`, and linefeed" $ do
      parseDef hardbreak "    +\n" `shouldParse` HardBreak

  describe "string parser" $ do
    it "parses normal string" $ do
      parseDef str "hello" `shouldParse` (Str "hello")
    it "fails on newline" $ do
      parseDef str `shouldFailOn` "\n"
    it "fails on empty string" $ do
      parseDef str `shouldFailOn` ""
    it "stops before `+` characters" $
      parseDef str "ab+c" `shouldParse` (Str "ab")

  describe "symbol parser" $ do
    it "parses a `+` character" $
      parseDef symbol "+" `shouldParse` (Str "+")
    it "fails on an alphanum character" $ do
      parseDef symbol `shouldFailOn` "a"
      parseDef symbol `shouldFailOn` "1"
    it "fails on an empty input" $ do
      parseDef symbol `shouldFailOn` ""

  describe "strong parser" $ do
    it "parses text between asterisks as strong" $
      parseDef strong "*strong*" `shouldParse` (Strong [Str "strong"])
    it "fails if opening asterisk is succeded by space" $
      parseDef strong `shouldFailOn` "* not strong*"
    it "fails if closing asterisk is preceded by space" $
      parseDef strong `shouldFailOn` "*not strong *"
    it "fails if right after string" $ do
      parseDef (str *> strong) `shouldFailOn` "str*strong*"
    it "fails if its follow by a string" $
      parseDef (strong *> str) `shouldFailOn` "*strong*str"
    it "doesn't consume anything if it fails" $ do
      parseDef (strong <|> symbol) "*notStrong" `shouldParse` (Str "*")
      parseDef (strong <|> symbol *> symbol *> str) "**notStrong"
        `shouldParse` (Str "notStrong")
    it "parses text delimited by double asterisk as strong" $
      parseDef strong "**strong**" `shouldParse` (Strong [Str "strong"])
    it "parses double-delimited text even if preceded by string" $
      parseDef (str *> strong) "str**strong**" `shouldParse`
        Strong [Str "strong"]

  describe "emphasis parser" $ do
    it "parses text between underscores as emphasized" $
      parseDef emphasis "_emph_" `shouldParse` (Emphasis [Str "emph"])
    it "treats umlaut characters as alpha-numeric" $
      parseDef (emphasis *> str) `shouldFailOn` "_nope_ä"

  describe "superscript parser" $ do
    it "reads superscript" $
      parseDef superscript "^super^" `shouldParse` (Superscript [Str "super"])
    it "reads superscript even when enclosed by words" $
      parseDef (str *> superscript <* str) "word^super^word" `shouldParse`
        (Superscript [Str "super"])

  describe "subscript parser" $ do
    it "reads subscript" $
      parseDef subscript "~sub~" `shouldParse` (Subscript [Str "sub"])
    it "reads subscript even when enclosed by words" $
      parseDef (str *> subscript <* str) "word~sub~word" `shouldParse`
        (Subscript [Str "sub"])

  describe "nested inlines" $ do
    it "allows emphasis to be nested in strong text" $
      parseDef strong "*__nested__*" `shouldParse`
        Strong [Emphasis [Str "nested"]]
    it "allows strong to be nested in emphasized text" $
      parseDef emphasis "_*nested*_" `shouldParse`
        Emphasis [Strong [Str "nested"]]

  describe "inlines in succession" $ do
    it "parses emphasized directly succeded by strong text" $
      parseDef (emphasis *> strong) "_emph_*strong*" `shouldParse`
        Strong [Str "strong"]
    it "parses strong directly succeded by emphasized text" $
      parseDef (strong *> emphasis) "*strong*_emph_" `shouldParse`
        Emphasis [Str "emph"]

  describe "inlineElement parser" $ do
    it "doesn't parse blank lines" $ do
      parseDef inlineElement `shouldFailOn` "\n\n"

  describe "url parser" $ do
    it "parses a simple url without a path" $ do
      parseDef url "http://pandoc.org" `shouldParse` "http://pandoc.org"
    it "parses URLs with paths" $ do
      parseDef url "http://asciidoctor.org/docs/user-manual/#elements" `shouldParse`
        "http://asciidoctor.org/docs/user-manual/#elements"
    it "stops at whitespace or opening square brackets" $ do
      parseDef (url *> whitespace) `shouldSucceedOn` "http://asciidoctor.org/docs "
      parseDef (url *> symbol) `shouldSucceedOn` "http://asciidoctor.org/docs["

  describe "link parser" $ do
    it "parses a simple link" $ do
      parseDef link "https://ccc.de/[*Chaos* Computer Club]" `shouldParse`
        Link "https://ccc.de/" [ Strong [Str "Chaos"]
                               , Space, Str "Computer"
                               , Space, Str "Club"]
