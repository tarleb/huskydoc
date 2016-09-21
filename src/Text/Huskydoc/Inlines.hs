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
{-# LANGUAGE LambdaCase #-}
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
  ( inlineElement
  , inlines
  , inlinesWithinLine
  -- Single inline parsers
  , emphasis
  , hardbreak
  , image
  , link
  , monospaced
  , monospacedInlines
  , monospacedPassThrough
  , softbreak
  , str
  , strong
  , subscript
  , superscript
  , symbol
  , whitespace
  -- helpers
  , quotedText
  , url
  ) where

import Control.Monad ( guard, void )
import Data.Char ( isSpace )
import Data.Maybe ( fromMaybe )
import Data.List ( (\\), intercalate )
import Data.Monoid ( (<>) )
import Data.Text (Text, pack, singleton )
import GHC.Exts ( IsList(..) )
import Text.Huskydoc.Attributes
import Text.Huskydoc.Parsing
import Text.Huskydoc.Patterns

-- | Parse one or more inline elements
inlines :: Parser Inlines
inlines = fromList <$> some inlineElement

-- | Parse one or more inlines, excluding some inline element parsers
inlinesExcluding :: [InlineParser] -> Parser Inlines
inlinesExcluding excl = fromList <$> some (inlineElementExcluding excl)

-- | Parse one or more non-newline consuming inlines
inlinesWithinLine :: Parser Inlines
inlinesWithinLine = inlinesExcluding [HardBreakParser, SoftBreakParser]

-- | One or more inlines surrounded but @start@ and @end@
inlinesBetween :: Parser a -> Parser b -> Parser Inlines
inlinesBetween start end = fromList <$> (start *> someTill inlineElement end)

-- | Inline parser enum, defined in the order in which the respective parsers
-- should be tried.
data InlineParser =
    HardBreakParser
  | WhitespaceParser
  | SoftBreakParser
  | StrongParser
  | EmphasisParser
  | MonoSpacedParser
  | SubscriptParser
  | SuperscriptParser
  | LinkParser
  | ImageParser
  | StrParser
  | SymbolParser
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Inline parser methods in the order in which they should be tried.
inlineParsers :: [Parser InlineElement]
inlineParsers = map inlineParser [minBound ..]

-- | Get the corresponding parser method for the given parser enum.
inlineParser :: InlineParser -> Parser InlineElement
inlineParser = \case
  EmphasisParser    -> emphasis
  HardBreakParser   -> hardbreak
  ImageParser       -> image
  LinkParser        -> link
  MonoSpacedParser  -> monospaced
  SoftBreakParser   -> softbreak
  StrParser         -> str
  StrongParser      -> strong
  SubscriptParser   -> subscript
  SuperscriptParser -> superscript
  SymbolParser      -> symbol
  WhitespaceParser  -> whitespace

-- | Parse a single inline element.
inlineElement :: Parser InlineElement
inlineElement = label "inline element" $ choice inlineParsers

inlineElementExcluding :: [InlineParser] -> Parser InlineElement
inlineElementExcluding = choice . map inlineParser . ([minBound ..] \\)

-- | Parse one or more whitespace characters (i.e. tabs or spaces).
whitespace :: Parser InlineElement
whitespace = Space <$ someSpaces

-- | Parse a hard linebreak.
hardbreak :: Parser InlineElement
hardbreak = HardBreak <$ try (someSpaces *> char '+' *> skipSpaces *> eol)

-- | Parse a soft linebreak.
softbreak :: Parser InlineElement
softbreak = SoftBreak <$ try inlinesBreak

-- | Parse the end of a line only if the next line contains inlines
inlinesBreak :: Parser ()
inlinesBreak = try $ skipSpaces <* void eol
  <* notFollowedBy blankline
  <* notFollowedBy (char '+' *> eol) -- list continuation
  <* notFollowedBy (skipSpaces *> some (oneOf ("-*"::String)) *> someSpaces) -- List item
  <* notFollowedBy (skipSpaces *> some (char '.') *> someSpaces) -- Ordered-list item
  <* notFollowedBy (string "|===") -- table delimiter

-- | Parse monospaced text
monospaced :: Parser InlineElement
monospaced = label "monospaced element" $
  monospacedPassThrough <|> monospacedInlines

monospacedInlines :: Parser InlineElement
monospacedInlines = label "monospaced inlines" . try $
  quotedText RichMonospaced '+'

monospacedPassThrough :: Parser InlineElement
monospacedPassThrough = label "monospaced pass-through" $
  monospacedUnconstrainedPassThrough <|> monospacedConstrainedPassThrough

monospacedUnconstrainedPassThrough :: Parser InlineElement
monospacedUnconstrainedPassThrough = label "monospaced unconstrained" . try $
  let attribs = fromMaybe nullAttributes <$> optional attributes
      txt = pack <$> (string "``" *> someTill anyChar (string "``"))
  in RichMonospaced <$> attribs <*> (fromList . (:[]) . Str <$> txt)

monospacedConstrainedPassThrough :: Parser InlineElement
monospacedConstrainedPassThrough =
  label "constrained monospaced pass-through" $ try $ do
  guard =<< notAfterString
  attribs <- fromMaybe nullAttributes <$> optional attributes
  char '`'
  notFollowedBy spaceChar
  txt <- pack <$> someTill constrainedChar end
  return $ RichMonospaced attribs (fromList [Str txt])
  where end = char '`' *> notFollowedBy alphaNumChar <|> eof
        constrainedChar = (satisfy (not . isSpace))
                          <|> (try $ satisfy isSpace <* notFollowedBy (char '`'))

-- | Parse a simple, markup-less string.
str :: Parser InlineElement
str = Str . pack <$> some (noneOf disallowedStrChars) <* markEndOfStr

-- | Parse text marked-up as strong.
strong :: Parser InlineElement
strong = quotedText RichStrong '*'

-- | Parse text marked-up as emphasized
emphasis :: Parser InlineElement
emphasis = quotedText RichEmphasis '_'

-- | Parse text marked-up as superscript
superscript :: Parser InlineElement
superscript = unconstrainedQuotedText RichSuperscript (char '^')

-- | Parse text marked-up as subscript
subscript :: Parser InlineElement
subscript = unconstrainedQuotedText RichSubscript (char '~')

quotedText :: (Attributes -> Inlines -> InlineElement)
           -> Char
           -> Parser InlineElement
quotedText bldr c = (unconstrainedQuotedText bldr (try $ string [c,c])
                     <|> constrainedQuotedText bldr (char c))
                    <* markEndOfDelimitedElement

-- | Parse a constrained quoted text, i.e. text that follows basic rules like
-- the requirement to not be surrounded by words.
constrainedQuotedText :: (Attributes -> Inlines -> InlineElement)
                      -> Parser a
                      -> Parser InlineElement
constrainedQuotedText bldr delimiter = try $ do
  guard =<< notAfterString
  attribs <- optional attributes
  delimiter
  notFollowedBy spaceChar
  elements <- someTill inlineElement endChar
  return $ bldr (fromMaybe nullAttributes attribs) (fromList elements)
  where
    endChar = do
      guard =<< ((||) <$> isAfterString <*> isAfterDelimitedElement)
      delimiter
      notFollowedBy alphaNumChar <|> eof

unconstrainedQuotedText :: (Attributes -> Inlines -> InlineElement)
                        -> Parser a
                        -> Parser InlineElement
unconstrainedQuotedText bldr delimiter = try $ do
  attribs <- optional attributes
  delimiter
  elements <- someTill inlineElement delimiter
  return $ bldr (fromMaybe nullAttributes attribs) (fromList elements)

--
-- Images
--
image :: Parser InlineElement
image = try $ do
  let disallowedSourceChars = "[] " <> specialCharacters :: String
  string "image:"
  src <- pack <$> manyTill (noneOf disallowedSourceChars) (lookAhead $ char '[')
  attribs <- imageAttributes
  return $ RichImage attribs src

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
symbol = do
  forbiddenChars' <- forbiddenChars
  let allowedChars = filter (`notElem` forbiddenChars') markupDelimiterCharacters
  Str . singleton <$> oneOf allowedChars

-- | Get a list of characters that may not be be parsed as parts of inline
-- elements. The list changes depending on the current context.
forbiddenChars :: Parser String
forbiddenChars = foldr addCharsForbiddenInContext [] <$> getParserContexts
  where
    addCharsForbiddenInContext :: ParserContext -> [Char] -> [Char]
    addCharsForbiddenInContext ctx cs =
      case ctx of
        TableContext -> '|':cs
        ListContextWithMarker (x:_) -> x:cs
        _            -> cs

specialCharacters :: String
specialCharacters =
  [ '\t' -- space (whitespace)
  , ' '  -- space (whitespace)
  , '\r' -- part of CRLF (hardbreak, softbreak)
  , '\n' -- line breaks (hardbreak, softbreak)
  ]

-- | Markup relevant characters in ASCII order
markupDelimiterCharacters :: String
markupDelimiterCharacters =
  [ '*'  -- opening/closing character for strong
  , '+'  -- continuation marker, part of hardbreaks
  , '['  -- beginning of attributes or link description
  , ']'  -- end of attributes or link description
  , '^'  -- opening/closing character for superscript
  , '_'  -- opening/closing character for emphasis
  , '`'  -- opening/closing character for monospaced
  , '|'  -- columns delimiter
  , '~'  -- opening/closing character for subscript
  ]

disallowedStrChars :: String
disallowedStrChars = (specialCharacters ++ markupDelimiterCharacters)
