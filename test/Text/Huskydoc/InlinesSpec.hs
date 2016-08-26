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
Module      :  Text.Huskydoc.Inlines
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

import Text.Huskydoc.Inlines
import Text.Huskydoc.Parsing ( parseDef, (<|>) )

import Data.Text
import Test.Hspec
import Test.Hspec.Megaparsec

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

    describe "hard linebreaks parser" $ do
        it "parses spaces, continuation char `+`, and crlf" $ do
            parseDef hardbreak " +\r\n" `shouldParse` LineBreak
        it "parses spaces,  continuation char `+`, and linefeed" $ do
            parseDef hardbreak "    +\n" `shouldParse` LineBreak

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

    describe "strong parser" $ do
        it "parses text between asterisks as strong" $
            parseDef strong "*strong*" `shouldParse` (Strong [Str "strong"])
        it "fails if opening underscore is followed by space" $
            parseDef strong `shouldFailOn` "* not strong*"
        it "fails if right after string" $ do
            parseDef (str *> strong) `shouldFailOn` "str*strong*"
        it "fails if its follow by a string" $
            parseDef (strong *> str) `shouldFailOn` "*strong*str"
        it "doesn't consume anything if it fails" $ do
            parseDef (strong <|> symbol) "*notStrong" `shouldParse` (Str "*")
            parseDef (strong <|> symbol *> symbol *> str) "**notStrong"
                     `shouldParse` (Str "notStrong")
        it "parses text delimited by double underscore as strong" $
            parseDef strong "**strong**" `shouldParse` (Strong [Str "strong"])
        it "parses double-delimited text even if preceded by string" $
            parseDef (str *> strong) "str**strong**" `shouldParse` (Strong [Str "strong"])
