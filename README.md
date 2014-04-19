## Synopsis

**mk-note-file-name** is a small emacs lisp library for building
unique note file names consisting of the current date, followed by a
letter suffix (building a positional numeral sytem to the base 26
where each digit is mapped to the letters of the alphabet `[a-z]`
where the first letter 'a' is the identity element under addition),
followed by keywords (separated by spaces) and a file extension.
