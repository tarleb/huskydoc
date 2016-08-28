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
            parseDef softbreak "\r\n" `shouldParse` softBreak
        it "parses linefeed" $ do
            parseDef softbreak "\n" `shouldParse` softBreak
        it "allows spaces before linefeed" $ do
            parseDef softbreak "   \n" `shouldParse` softBreak

    describe "hard linebreaks parser" $ do
        it "parses spaces, continuation char `+`, and crlf" $ do
            parseDef hardbreak " +\r\n" `shouldParse` hardBreak
        it "parses spaces,  continuation char `+`, and linefeed" $ do
            parseDef hardbreak "    +\n" `shouldParse` hardBreak

    describe "string parser" $ do
        it "parses normal string" $ do
            parseDef str "hello" `shouldParse` (bstr "hello")
        it "fails on newline" $ do
            parseDef str `shouldFailOn` "\n"
        it "fails on empty string" $ do
            parseDef str `shouldFailOn` ""
        it "stops before `+` characters" $
            parseDef str "ab+c" `shouldParse` (bstr "ab")

    describe "symbol parser" $ do
        it "parses a `+` character" $
            parseDef symbol "+" `shouldParse` (bstr "+")
        it "fails on an alphanum character" $ do
            parseDef symbol `shouldFailOn` "a"
            parseDef symbol `shouldFailOn` "1"

    describe "strong parser" $ do
        it "parses text between asterisks as strong" $
            parseDef strong "*strong*" `shouldParse` (bstrong [bstr "strong"])
        it "fails if opening asterisk is succeded by space" $
            parseDef strong `shouldFailOn` "* not strong*"
        it "fails if closing asterisk is preceded by space" $
            parseDef strong `shouldFailOn` "*not strong *"
        it "fails if right after string" $ do
            parseDef (str *> strong) `shouldFailOn` "str*strong*"
        it "fails if its follow by a string" $
            parseDef (strong *> str) `shouldFailOn` "*strong*str"
        it "doesn't consume anything if it fails" $ do
            parseDef (strong <|> symbol) "*notStrong" `shouldParse` (bstr "*")
            parseDef (strong <|> symbol *> symbol *> str) "**notStrong"
                     `shouldParse` (bstr "notStrong")
        it "parses text delimited by double asterisk as strong" $
            parseDef strong "**strong**" `shouldParse` (bstrong [bstr "strong"])
        it "parses double-delimited text even if preceded by string" $
            parseDef (str *> strong) "str**strong**" `shouldParse` (bstrong [bstr "strong"])

    describe "emphasis parser" $ do
        it "parses text between underscores as emphasized" $
            parseDef emphasis "_emph_" `shouldParse` (bemphasis [bstr "emph"])
        it "treats umlaut characters as alpha-numeric" $
            parseDef (emphasis *> str) `shouldFailOn` "_nope_ä"

    -- describe "quoted text parser" $ do
    --      it "accepts preceding attributes" $
    --         parseDef (quotedText '*') `shouldSucceedOn` "[small]*wins*"

    describe "nested inlines" $ do
        it "allows emphasis to be nested in strong text" $
            parseDef strong "*__nested__*" `shouldParse` (bstrong [bemphasis [bstr "nested"]])
        it "allows strong to be nested in emphasized text" $
            parseDef emphasis "_*nested*_" `shouldParse` (bemphasis [bstrong [bstr "nested"]])

    describe "inlines in succession" $ do
        it "parses emphasized directly succeded by strong text" $
            parseDef (emphasis *> strong) "_emph_*strong*" `shouldParse` (bstrong [bstr "strong"])
        it "parses strong directly succeded by emphasized text" $
            parseDef (strong *> emphasis) "*strong*_emph_" `shouldParse` (bemphasis [bstr "emph"])

    describe "inlineElement parser" $ do
        it "doesn't parse blank lines" $ do
            parseDef inlineElement `shouldFailOn` "\n\n"

            
