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

import Data.Text
import Text.Huskydoc.Blocks (blocks)
import Text.Huskydoc.Parsing ((<?>), Parser, HuskydocError, parseDef)
import Text.Huskydoc.Patterns

-- | Parse a complete AsciiDoc document
document :: Parser Document
document = Document <$> pure emptyMeta <*> blocks <?> "document"

readAsciidoc :: Text -> Either HuskydocError Document
readAsciidoc input = parseDef document input
