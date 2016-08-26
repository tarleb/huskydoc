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
Module      :  Text.Huskydoc.Parsing
Copyright   :  © 2016 Albert Krewinkel
License     :  ISC

Maintainer  :  Albert Krewinkel <tarleb@zeitkraut.de>
Stability   :  experimental
Portability :  portable

Tests for the Parsing module.
-}
module Text.Huskydoc.Parsing
    ( blankline
    , skipSpaces
    , someSpaces
    , spaceChar
    -- Re-export Megaparsec types
    , Parser
    , module Text.Megaparsec
    ) where

import Control.Monad ( void )
import Data.Text
import Text.Megaparsec hiding ( spaceChar, spaces )
import Text.Megaparsec.Text ( Parser )

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
