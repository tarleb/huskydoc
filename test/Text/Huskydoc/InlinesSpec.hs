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
import Text.Huskydoc.Parsing (Parser, ParseError, parse)

import Data.Text
import Test.Hspec
import Test.Hspec.Megaparsec

-- | Run this spec.
main :: IO ()
main = hspec spec

-- | Helper function to test parsers, sets source name to the empty string.
parse' :: Parser a -> Text -> Either ParseError a
parse' p txt = parse p "" txt

spec :: Spec
spec = do
    describe "soft linebreaks parser" $ do
        it "parses crlf" $ do
            parse' softbreak "\r\n" `shouldParse` SoftBreak
        it "parses linefeed" $ do
            parse' softbreak "\n" `shouldParse` SoftBreak
        it "allows spaces before linefeed" $ do
            parse' softbreak "   \n" `shouldParse` SoftBreak

    describe "hard linebreaks parser" $ do
        it "parses spaces, continuation char `+`, and crlf" $ do
            parse' hardbreak " +\r\n" `shouldParse` LineBreak
        it "parses spaces,  continuation char `+`, and linefeed" $ do
            parse' hardbreak "    +\n" `shouldParse` LineBreak

    describe "string parser" $ do
        it "parses normal string" $ do
            parse' str "hello" `shouldParse` (Str "hello")
        it "fails on newline" $ do
            parse' str `shouldFailOn` "\n"
        it "fails on empty string" $ do
            parse' str `shouldFailOn` ""
