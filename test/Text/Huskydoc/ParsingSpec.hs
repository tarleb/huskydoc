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
Module      :  Text.Huskydoc.ParsingSpec
Copyright   :  © 2016 Albert Krewinkel
License     :  ISC

Maintainer  :  Albert Krewinkel <tarleb@zeitkraut.de>
Stability   :  experimental
Portability :  portable
-}
module Text.Huskydoc.ParsingSpec
    ( main
    , spec
    ) where

import Text.Huskydoc.Parsing

import Data.Text
import Test.Hspec
import Test.Hspec.Megaparsec

-- | Run this spec.
main :: IO ()
main = hspec spec

-- | Specifications for Parsing functions.
spec :: Spec
spec = do
    describe "spaceChar parser" $ do
        it "parses tab" $ do
            parseDef spaceChar "\t" `shouldParse` '\t'
        it "parses space" $ do
            parseDef spaceChar " " `shouldParse` ' '
        it "doesn't parse newline characters" $ do
            parseDef spaceChar `shouldFailOn` "\n"

    describe "skipSpaces" $ do
        it "parses single space char" $ do
            parseDef skipSpaces `shouldSucceedOn` " "
        it "parses many tabs and spaces" $ do
            parseDef skipSpaces `shouldSucceedOn` "  \t  \t\t"
        it "succeeds on empty string" $ do
            parseDef skipSpaces `shouldSucceedOn` ""

    describe "someSpaces" $ do
        it "parses single space char" $ do
            parseDef someSpaces `shouldSucceedOn` " "
        it "parses many tabs and spaces" $ do
            parseDef someSpaces `shouldSucceedOn` "  \t  \t\t"
        it "fails on empty string" $ do
            parseDef someSpaces `shouldFailOn` ""

    describe "blankline" $ do
        it "parses empty line plus final newline" $ do
            parseDef blankline `shouldSucceedOn`" \n"
        it "fails on non-empty line" $ do
            parseDef blankline `shouldFailOn` "   a \n"
