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
Module      :  Text.Huskydoc.Document
Copyright   :  © 2016 Albert Krewinkel
License     :  ISC

Maintainer  :  Albert Krewinkel <tarleb@zeitkraut.de>
Stability   :  experimental
Portability :  portable

Parsers for AsciiDoc documents
-}
module Text.Huskydoc.Document
  ( document
  , readAsciidoc
  ) where

import Control.Monad ( guard, void )
import Data.Text ( Text, pack )
import Text.Huskydoc.Blocks ( blocks, sectionTitle )
import Text.Huskydoc.Parsing
import Text.Huskydoc.Patterns

readAsciidoc :: Text -> Either HuskydocError Document
readAsciidoc input = parseDef document input

-- | Parse a complete AsciiDoc document
document :: Parser Document
document = label "document" $ do
  optional documentTitle
  blks <- blocks
  meta <- getMetadata
  return $ Document meta blks

--
-- Document title and metadata
--

-- | Parse a document title. This is inefficient, but should be called at most
-- once for each document.
documentTitle :: Parser ()
documentTitle = label "document title" . try $ do
  SectionTitle level titleInlns <- sectionTitle <*> pure nullAttributes
  guard (level == 0)
  (author, email, revision) <- (,,) <$> optional documentAuthor
                                    <*> optional documentEmail
                                    <*> optional documentRevision
  modifyLocalState $ \st ->
    let metadata = stateMetadata st
    in st { stateMetadata =
              metadata { metadataTitle = titleInlns
                       , metadataAuthor = author
                       , metadataEmail = email
                       , metadataRevision = revision
                       }
          }

documentAuthor :: Parser Text
documentAuthor = label "document author" . try $
  pack <$> someTill (noneOf ("\n\r"::String))
           (blankline <|> void (lookAhead documentEmail))

documentEmail :: Parser Text
documentEmail = label "document email" . try $ do
  char '<'
  pack <$> someTill anyChar (char '>') <* skipSpaces <* eol

documentRevision :: Parser Text
documentRevision = return mempty
