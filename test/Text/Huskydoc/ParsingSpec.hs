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
import Text.Megaparsec ( parse )

-- | Run this spec.
main :: IO ()
main = hspec spec

-- | Helper function to test parsers, sets source name to the empty string.
parse' :: Parser a -> Text -> Either ParseError a
parse' p txt = parse p "" txt

spec :: Spec
spec = do
    describe "spaceChar parser" $ do
        it "parses tab" $ do
            parse' spaceChar "\t" `shouldParse` '\t'
        it "parses space" $ do
            parse' spaceChar " " `shouldParse` ' '
        it "doesn't parse newline characters" $ do
            parse' spaceChar `shouldFailOn` "\n"

    describe "skipSpaces" $ do
        it "parses single space char" $ do
            parse' skipSpaces `shouldSucceedOn` " "
        it "parses many tabs and spaces" $ do
            parse' skipSpaces `shouldSucceedOn` "  \t  \t\t"
        it "succeeds on empty string" $ do
            parse' skipSpaces `shouldSucceedOn` ""

    describe "blankline" $ do
        it "parses empty line plus final newline" $ do
            parse' blankline `shouldSucceedOn`" \n"
        it "fails on non-empty line" $ do
            parse' blankline `shouldFailOn` "   a \n"

