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

Parsers for inline elements
-}
module Text.Huskydoc.Inlines
    ( Inline (..)
    , InlineElement
    , Inlines (..)
    , inlineElement
    , inlines
    -- Single inline parsers
    , emphasis
    , hardbreak
    , softbreak
    , str
    , strong
    , symbol
    , whitespace
    -- builders
    , softBreak
    , hardBreak
    , bstr
    , bstrong
    , strongWith
    , bemphasis
    , emphasisWith
    -- helpers
    , quotedText
    ) where

import           Text.Huskydoc.Attributes
import           Text.Huskydoc.Parsing
import           Control.Monad ( guard, void )
import           Data.Maybe ( fromMaybe )
import           Data.Sequence ( Seq )
import qualified Data.Sequence as Seq
import           Data.Text

-- | Inline text types.
data Inline =
      Emphasis [InlineElement]
    | LineBreak
    | SoftBreak
    | Space
    | Str Text
    | Strong [InlineElement]
    deriving (Show, Eq, Ord)

type InlineElement = RichElement Inline

newtype Inlines = Inlines { fromInlines :: Seq InlineElement }
    deriving (Show, Eq, Ord)

inlines :: Parser Inlines
inlines = Inlines . Seq.fromList <$> some inlineElement

-- | Parse a single inline element.
inlineElement :: Parser InlineElement
inlineElement = choice
         [ hardbreak
         , whitespace
         , softbreak
         , strong
         , emphasis
         , str
         , symbol
         ] <?> "inline element"

--
-- Builders
--

bstrong :: [InlineElement] -> InlineElement
bstrong = plainElement . Strong

strongWith :: Attributes -> [InlineElement] -> InlineElement
strongWith a es = richElement a (Strong es)

bemphasis :: [InlineElement] -> InlineElement
bemphasis = plainElement . Emphasis

emphasisWith :: Attributes -> [InlineElement] -> InlineElement
emphasisWith a es = richElement a (Emphasis es)

bstr :: Text -> InlineElement
bstr = plainElement . Str

softBreak :: InlineElement
softBreak = plainElement SoftBreak

hardBreak :: InlineElement
hardBreak = plainElement LineBreak

-- | Parse one or more whitespace characters (i.e. tabs or spaces).
whitespace :: Parser InlineElement
whitespace = plainElement Space <$ someSpaces

-- | Parse a hard linebreak.
hardbreak :: Parser InlineElement
hardbreak = plainElement LineBreak
            <$ try (someSpaces *> char '+' *> skipSpaces *> eol)

-- | Parse a soft linebreak.
softbreak :: Parser InlineElement
softbreak = plainElement SoftBreak
            <$ try (skipSpaces *> void eol *> notFollowedBy blankline)

-- | Parse a simple, markup-less string.
str :: Parser InlineElement
str = bstr . pack <$> some (noneOf disallowedStrChars) <* markEndOfStr

-- | Parse text marked-up as strong.
strong :: Parser InlineElement
strong = quotedText Strong '*'

-- | Parse text marked-up as emphasized
emphasis :: Parser InlineElement
emphasis = quotedText Emphasis '_'

quotedText :: ([InlineElement] -> Inline) -> Char -> Parser InlineElement
quotedText constr c = (doubleDelimitedMarkup c <|> singleDelimitedMarkup c)
                    <* markEndOfDelimitedElement
  where
    singleDelimitedMarkup :: Char -> Parser InlineElement
    singleDelimitedMarkup c' = try $ do
        guard =<< notAfterString
        attributes <- optional parseAttributes
        char c'
        notFollowedBy spaceChar
        element <- constr <$> someTill inlineElement (try endChar)
        return $ richElement (fromMaybe nullAttributes attributes) element
      where
        endChar = do
          guard =<< ((||) <$> isAfterString <*> isAfterDelimitedElement)
          char c'
          notFollowedBy alphaNumChar <|> eof

    doubleDelimitedMarkup :: Char -> Parser InlineElement
    doubleDelimitedMarkup c' = try $ do
        attributes <- optional parseAttributes
        string [c',c']
        element <- constr <$> someTill inlineElement (try $ string [c',c'])
        return $ richElement (fromMaybe nullAttributes attributes) element

-- | Parse a single special character.
symbol :: Parser InlineElement
symbol = bstr . pack . (:[]) <$> oneOf markupDelimiterCharacters

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
