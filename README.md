## Synopsis

**mk-note-file-name** is a small emacs lisp library for building
unique note file names consisting of the current date, followed by a
letter suffix (building a positional numeral sytem to the base 26
where each digit is mapped to the letters of the alphabet `[a-z]`
where the first letter `a` is the identity element under addition),
followed by keywords (separated by spaces) and a file extension.

## Examples
Creating note file names:
```lisp
(setq dir (expand-file-name "~/Documents"))
(find-file (mg/note-file-name-make-unique-file-name dir ".markdown" "foo" "bar"))
=> "/Users/someuser/Documents/20140419a foo bar.markdown"
(find-file (mg/note-file-name-make-unique-file-name dir ".markdown" "foo" "bar"))
=> "/Users/someuser/Documents/20140419b foo bar.markdown"
```

Converting natual numbers to base 26 positional numeral system:
```lisp
(mapcar 'mg/note-file-name-toalphabet '(0 1 2 3 4 5 6 7 8 9))
=> ("a" "b" "c" "d" "e" "f" "g" "h" "i" "j")
```

EOF
