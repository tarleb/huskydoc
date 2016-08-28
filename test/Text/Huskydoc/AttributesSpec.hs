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
Module      :  Text.Huskydoc.AttributesSpec
Copyright   :  © 2016 Albert Krewinkel
License     :  ISC

Maintainer  :  Albert Krewinkel <tarleb@zeitkraut.de>
Stability   :  experimental
Portability :  portable

Tests for the attribute parsers.
-}
module Text.Huskydoc.AttributesSpec
    ( main
    , spec
    ) where

import Text.Huskydoc.Attributes
import Text.Huskydoc.Parsing ( parseDef )

import Test.Hspec
import Test.Hspec.Megaparsec

-- | Run this spec.
main :: IO ()
main = hspec spec

-- | Specifications for Attributes parsing functions.
spec :: Spec
spec = do
    describe "nameAttr" $ do
        it "parses a simple named attribute" $ do
            parseDef namedAttr "foo=\"bar\"" `shouldParse` (NamedAttr "foo" "bar")

    describe "positionalAttr" $ do
        it "parses a space-less word" $ do
            parseDef positionalAttr "verse" `shouldParse` (PositionalAttr "verse")
        it "strips surrounding spaces" $ do
            parseDef positionalAttr "  verse  " `shouldParse` (PositionalAttr "verse")

    describe "parseAttributes" $ do
        it "parses a single positional attribute" $ do
            parseDef parseAttributes "[verse]"
                 `shouldParse` (toAttributes [PositionalAttr "verse"])
        it "parses a single named attribute" $ do
            parseDef parseAttributes "[role=\"verse\"]"
                `shouldParse` (toAttributes [NamedAttr "role" "verse"])
        it "parses a many comma-separated positional attributes" $ do
            parseDef parseAttributes "[verse,rick, roll ]" `shouldParse`
                (toAttributes
                 [ PositionalAttr "verse"
                 , PositionalAttr "rick"
                 , PositionalAttr "roll"
                 ])
        it "parses a many comma-separated named attributes" $ do
            parseDef parseAttributes "[quality=\"medium\", summer=\"hot\", drinks=\"cool\"]"
                `shouldParse` (toAttributes
                     [ NamedAttr "quality" "medium"
                     , NamedAttr "summer" "hot"
                     , NamedAttr "drinks" "cool"
                     ])
