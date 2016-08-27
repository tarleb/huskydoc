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

{-|
Module      :  Text.Huskydoc.Inlines
Copyright   :  © 2016 Albert Krewinkel
License     :  ISC

Maintainer  :  Albert Krewinkel <tarleb@zeitkraut.de>
Stability   :  experimental
Portability :  portable

Tests for the inlines parsers.
-}
module Text.Huskydoc.Inlines
    ( Inline (..)
    , inline
    -- Single inline parsers
    , emphasis
    , hardbreak
    , softbreak
    , str
    , strong
    , symbol
    , whitespace
    ) where

import Text.Huskydoc.Parsing
import Control.Monad ( guard, void )
import Data.Text

-- | Inline text types.
data Inline =
      Emphasis [Inline]
    | LineBreak
    | SoftBreak
    | Space
    | Str Text
    | Strong [Inline]
        deriving (Show, Eq)

-- | Parse a single inline element.
inline :: Parser Inline
inline = choice
         [ hardbreak
         , whitespace
         , softbreak
         , strong
         , emphasis
         , str
         , symbol
         ] <?> "inline"

-- | Parse one or more whitespace characters (i.e. tabs or spaces).
whitespace :: Parser Inline
whitespace = Space <$ someSpaces

-- | Parse a hard linebreak.
hardbreak :: Parser Inline
hardbreak = LineBreak <$ try (someSpaces *> char '+' *> skipSpaces *> eol)

-- | Parse a soft linebreak.
softbreak :: Parser Inline
softbreak = SoftBreak <$ try (skipSpaces *> void eol *> notFollowedBy blankline)

-- | Parse a simple, markup-less string.
str :: Parser Inline
str = Str . pack <$> some (noneOf disallowedStrChars) <* markEndOfStr

-- | Parse text marked-up as strong.
strong :: Parser Inline
strong = Strong <$> delimitedMarkup '*'

-- | Parse text marked-up as emphasized
emphasis :: Parser Inline
emphasis = Emphasis <$> delimitedMarkup '_'

delimitedMarkup :: Char -> Parser [Inline]
delimitedMarkup c = (doubleDelimitedMarkup c <|> singleDelimitedMarkup c)
                    <* markEndOfDelimitedElement
  where
    singleDelimitedMarkup :: Char -> Parser [Inline]
    singleDelimitedMarkup c = try $ do
        guard =<< notAfterString
        char c
        notFollowedBy spaceChar
        someTill inline (try endChar)
      where
        endChar = do
          guard =<< ((||) <$> isAfterString <*> isAfterDelimitedElement)
          char c
          notFollowedBy alphaNumChar <|> eof

    doubleDelimitedMarkup :: Char -> Parser [Inline]
    doubleDelimitedMarkup c = try $
        string [c,c] *> someTill inline (try $ string [c,c])

-- | Parse a single special character.
symbol :: Parser Inline
symbol = Str . pack . (:[]) <$> oneOf disallowedStrChars

specialCharacters :: String
specialCharacters =
    [ '\t' -- space (whitespace)
    , ' '  -- space (whitespace)
    , '\r' -- part of CRLF (hardbreak, softbreak)
    , '\n' -- line breaks (hardbreak, softbreak)
    ]

markupDelimiterCharacters :: String
markupDelimiterCharacters =
    [ '*'  -- opening/closing character for strong
    , '_'  -- opening/closing character for emphasis
    , '+'  -- continuation marker, part of hardbreaks
    ]

disallowedStrChars :: String
disallowedStrChars = (specialCharacters ++ markupDelimiterCharacters)
