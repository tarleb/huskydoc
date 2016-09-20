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
{-# LANGUAGE CPP #-}
{-|
Module      :  Text.Huskydoc.Parsing
Copyright   :  © 2016 Albert Krewinkel
License     :  ISC

Maintainer  :  Albert Krewinkel <tarleb@zeitkraut.de>
Stability   :  experimental
Portability :  portable

Tests for the Parsing module.
-}
module Text.Huskydoc.Parsing
  ( HuskydocError
  , Parser
  , ParserContext (..)
  , ParserState (..)
  , blankline
  , getMetadata
  , getParserContexts
  , isAfterString
  , isAfterDelimitedElement
  , isInContext
  , markEndOfStr
  , markEndOfDelimitedElement
  , modifyLocalState
  , notAfterString
  , parseDef
  , skipSpaces
  , someSpaces
  , spaceChar
  , withColumnCount
  , withContext
  -- Re-export Megaparsec types
  , module Text.Megaparsec
  ) where

import Control.Monad ( void )
import Control.Monad.Trans.Class (lift)
import Data.Default ( Default(..) )
import Data.Text ( Text )
import Text.Huskydoc.Types ( Metadata(..) )
import Text.Megaparsec hiding ( spaceChar )
import qualified Control.Monad.Trans.State as TransState

#if MIN_VERSION_megaparsec(5,0,0)
type Parser = ParsecT Dec Text (TransState.State ParserState)
type HuskydocError = ParseError (Token Text) Dec
#else
type Parser = ParsecT Text (TransState.State ParserState)
type HuskydocError = ParseError
#endif

-- | Parser state
data ParserState = ParserState
  { stateLastStrPos :: Maybe SourcePos -- ^ End position of the last Str
  , stateLastDelimitedElementPos :: Maybe SourcePos -- ^ End of the last
                                                    -- element delimited by
                                                    -- markup characters
  , stateMetadata       :: Metadata        -- ^ Meta information on the document
  , stateParserContexts :: [ParserContext] -- ^ Current parsing context; contexts
                                           -- entered first are listed last.
  }

instance Default ParserState where
  def = ParserState
        { stateLastStrPos = Nothing
        , stateLastDelimitedElementPos = Nothing
        , stateMetadata = mempty
        , stateParserContexts = []
        }

-- | Contexts the parser can be in.
data ParserContext =
    TableContext
  | ListContextWithMarker String
  deriving (Eq, Ord, Show)

-- | Parse in the given context
withContext :: ParserContext -> Parser a -> Parser a
withContext ctx p = do
  modifyLocalState (\st -> st { stateParserContexts = ctx:stateParserContexts st})
  res <- p
  modifyLocalState (\st -> st { stateParserContexts = drop 1 $ stateParserContexts st})
  return res

-- | Get the list of current parser contexts
getParserContexts :: Parser [ParserContext]
getParserContexts = lift $ stateParserContexts <$> TransState.get

isInContext :: ParserContext -> Parser Bool
isInContext ctx = (ctx `elem`) <$> getParserContexts

-- | Get metadata from parser state
getMetadata :: Parser Metadata
getMetadata = lift $ stateMetadata <$> TransState.get

-- | Helper function to test parsers.  This sets the source name to the empty
--   string and uses the default parser state.
parseDef :: Parser a -> Text -> Either HuskydocError a
parseDef p txt = flip TransState.evalState def $  runParserT p "" txt

modifyLocalState :: (ParserState -> ParserState) -> Parser ()
modifyLocalState = lift . TransState.modify

-- | Set end position of last string to current position.
markEndOfStr :: Parser ()
markEndOfStr = modifyLocalState . setLastStrPos =<< getPosition
  where setLastStrPos pos st = st { stateLastStrPos = Just pos }

markEndOfDelimitedElement :: Parser ()
markEndOfDelimitedElement =
  modifyLocalState . setLastDelimitedElement =<< getPosition
  where setLastDelimitedElement pos st =
            st { stateLastDelimitedElementPos = Just pos }

-- | Check whether the parser position is right after a str.
notAfterString :: Parser Bool
notAfterString = do
  pos <- getPosition
  st <- lift (stateLastStrPos <$> TransState.get)
  return $ st /= Just pos

isAfterString :: Parser Bool
isAfterString = do
  (==) <$> (Just <$> getPosition) <*> lift (stateLastStrPos <$> TransState.get)

isAfterDelimitedElement :: Parser Bool
isAfterDelimitedElement = do
  (==) <$> (Just <$> getPosition) <*> lift (stateLastDelimitedElementPos <$> TransState.get)


--
-- Space parsing
--

-- | Parses a space or tab.
spaceChar :: Parser Char
spaceChar = satisfy $ \c -> c == ' ' || c == '\t'

-- | Skip zero or more spaces.
skipSpaces :: Parser ()
skipSpaces = skipMany spaceChar

-- | Skip one or more spaces.
someSpaces :: Parser ()
someSpaces = skipSome spaceChar

-- | Skips zero or more spaces or tabs, then reads a newline.
blankline :: Parser ()
blankline = try (skipSpaces *> void eol)

-- | Returns result of and change in columns caused by @parser@.
withColumnCount :: Parser a -> Parser (Int, a)
withColumnCount p = do
  startPos <- colPos
  res <- p
  endPos <- colPos
  return (fromIntegral (endPos - startPos), res)

colPos :: Parser Int
#if MIN_VERSION_megaparsec(5,0,0)
colPos = fromIntegral . unPos . sourceColumn <$> getPosition
#else
colPos = sourceColumn <$> getPosition
#endif
