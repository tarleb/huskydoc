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
    , hardbreak
    , softbreak
    , str
    ) where

import Text.Huskydoc.Parsing
import Control.Monad ( void )
import Data.Text

-- | Inline text types.
data Inline =
      Str Text
    | Space
    | SoftBreak
    | LineBreak
        deriving (Show, Eq)

-- | Parse a single inline element.
inline :: Parser Inline
inline = choice
         [ hardbreak
         , whitespace
         , softbreak
         , str
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
str = Str . pack <$> some (noneOf " \t\n\r")
