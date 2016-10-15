Huskydoc
========

Parser for AsciiDoc documents.

[![license](https://img.shields.io/badge/license-ISC-brightgreen.svg)](https://opensource.org/licenses/isc-license)
[![travis build status](https://img.shields.io/travis/tarleb/huskydoc.svg)](https://travis-ci.org/tarleb/huskydoc)

Usage
-----

The main purpose of `huskydoc` is the transformation of AsciiDoc documents
into [Pandoc](http://pandoc.org) format. The document is transformed into a
pandoc-readable JSON structure. The following command uses `huskydoc` and
`pandoc` to transform an AsciiDoc document into LaTeX:

    huskydoc document.adoc | pandoc -f json -t latex

The important part of the above command is `-f json`, telling Pandoc which input
format to expect. See the [Pandoc manual](https://pandoc.org/MANUAL.html) for
more options.

License
-------

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
