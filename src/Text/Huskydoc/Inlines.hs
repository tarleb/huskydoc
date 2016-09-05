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
  ( InlineParser (..)
  , inlineParsers
  , inlineElement
  , inlines
  , inlinesExcluding
  -- Single inline parsers
  , emphasis
  , hardbreak
  , link
  , softbreak
  , str
  , strong
  , symbol
  , whitespace
  -- helpers
  , quotedText
  , url
  ) where

import Control.Monad ( guard, void )
import Data.Maybe ( fromMaybe )
import Data.List ( intercalate )
import Data.Monoid ( (<>) )
import Data.Text (Text, pack)
import Text.Huskydoc.Attributes
import Text.Huskydoc.Parsing
import Text.Huskydoc.Patterns

-- | Parse one or more inline elements
inlines :: Parser Inlines
inlines = toInlines <$> some inlineElement

-- | Parse one or more inlines, excluding some inline element parsers
inlinesExcluding :: [InlineParser] -> Parser Inlines
inlinesExcluding excl = toInlines <$> some (inlineElementExcluding excl)

-- | One or more inlines surrounded but @start@ and @end@
inlinesBetween :: Parser a -> Parser b -> Parser Inlines
inlinesBetween start end = toInlines <$> (start *> someTill inlineElement end)

data InlineParser =
    EmphasisParser
  | HardBreakParser
  | SoftBreakParser
  | StrParser
  | StrongParser
  | SymbolParser
  | WhitespaceParser
  deriving (Eq, Ord, Show)

-- | Inline parser methods, marked by @InlineParser@ type. The order of the
-- parsers is the order in which they should be tried.
inlineParsers :: [(InlineParser, Parser InlineElement)]
inlineParsers =
  [ (HardBreakParser, hardbreak)
  , (WhitespaceParser, whitespace)
  , (SoftBreakParser, softbreak)
  , (StrongParser, strong)
  , (EmphasisParser, emphasis)
  , (StrParser, str)
  , (SymbolParser, symbol)
  ]

-- | Parse a single inline element.
inlineElement :: Parser InlineElement
inlineElement = choice (map snd inlineParsers) <?> "inline element"

inlineElementExcluding :: [InlineParser] -> Parser InlineElement
inlineElementExcluding ps =
  choice . map snd . filter ((`notElem` ps) . fst) $ inlineParsers

-- | Parse one or more whitespace characters (i.e. tabs or spaces).
whitespace :: Parser InlineElement
whitespace = Space <$ someSpaces

-- | Parse a hard linebreak.
hardbreak :: Parser InlineElement
hardbreak = HardBreak <$ try (someSpaces *> char '+' *> skipSpaces *> eol)

-- | Parse a soft linebreak.
softbreak :: Parser InlineElement
softbreak = SoftBreak <$ try (skipSpaces *> void eol *> notFollowedBy blankline)

-- | Parse a simple, markup-less string.
str :: Parser InlineElement
str = Str . pack <$> some (noneOf disallowedStrChars) <* markEndOfStr

-- | Parse text marked-up as strong.
strong :: Parser InlineElement
strong = quotedText RichStrong '*'

-- | Parse text marked-up as emphasized
emphasis :: Parser InlineElement
emphasis = quotedText RichEmphasis '_'

quotedText :: (Attributes -> Inlines -> InlineElement)
           -> Char
           -> Parser InlineElement
quotedText bldr c = (doubleDelimitedMarkup c <|> singleDelimitedMarkup c)
                    <* markEndOfDelimitedElement
  where
    singleDelimitedMarkup :: Char -> Parser InlineElement
    singleDelimitedMarkup c' = try $ do
      guard =<< notAfterString
      attributes <- optional parseAttributes
      char c'
      notFollowedBy spaceChar
      elements <- someTill inlineElement (try endChar)
      return $ bldr (fromMaybe nullAttributes attributes) (toInlines elements)
      where
        endChar = do
          guard =<< ((||) <$> isAfterString <*> isAfterDelimitedElement)
          char c'
          notFollowedBy alphaNumChar <|> eof

    doubleDelimitedMarkup :: Char -> Parser InlineElement
    doubleDelimitedMarkup c' = try $ do
      attributes <- optional parseAttributes
      string [c',c']
      elements <- someTill inlineElement (try $ string [c',c'])
      return $ bldr (fromMaybe nullAttributes attributes) (toInlines elements)

-- | Parse a links
link :: Parser InlineElement
link = try $ Link <$> url <*> inlinesBetween (char '[') (char ']')

-- | Parse an URL
url :: Parser Text
url = try $ do
  schema <- (<>) <$> choice (map string schemas) <*> string "://"
  let subdomain = try $ manyTill (alphaNumChar <|> oneOf ("-_"::String)) (char '.')
  subdomains <- many subdomain
  tld <- many letterChar
  path <- many (notFollowedBy (spaceChar <|> char '[') *> anyChar)
  return . pack $ schema <> intercalate "." (subdomains <> [tld]) <> path

schemas :: [String]
schemas = ["https", "http", "ftp", "irc", "mailto"]

-- | Parse a single special character.
symbol :: Parser InlineElement
symbol = Str . pack . (:[]) <$> oneOf markupDelimiterCharacters

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
  , '['  -- beginning of attributes or link description
  , ']'  -- end of attributes or link description
  ]

disallowedStrChars :: String
disallowedStrChars = (specialCharacters ++ markupDelimiterCharacters)
