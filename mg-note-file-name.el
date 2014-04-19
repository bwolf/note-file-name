;;; mg-note-file-name.el --- Small library for dealing with note file names -*- lexical-binding: t -*-
;;; Commentary:
;;;    See the function documentations.
;;; Code:

(defun mg/note-file-name-tobase (d base digits)
  "Convert number D to positional numeral system.
BASE is the base of the numeral system and DIGITS specifies the
letters of numeral system.  The first letter is the identity
element under addition."
  (let ((r (mod d base)))
    (if (zerop (- d r))
        (substring digits r (+ 1 r))
      (concat (mg/note-file-name-tobase (/ (- d r) base) base digits) (substring digits r (+ 1 r))))))

(defun mg/note-file-name-toalphabet (d)
  "Convert given number D to positional numeral sytem of base 26.
The letters of the numeral system are the letters of the alphabet
\[a-z\] where the fist letter 'a' is the identity element under
addition."
  (mg/note-file-name-tobase d 26 "abcdefghijklmnopqrstuvwxyz"))

(defun mg/note-file-name-make-prefix (n)
  "Create string with date and positional numeral system of base 26 of number N."
  (concat (format-time-string "%Y%m%d") (mg/note-file-name-toalphabet n)))

(defun mg/note-file-name-make-unique-file-name (directory extension &rest keywords)
  "Make new note file name in DIRECTORY with EXTENSION and KEYWORDS.
The filename will be prefixed with the current date \"%Y%m%d\"
and is followed by a letter suffix which is build with
`mg/note-file-name-toalphabet' such that the date and the letter suffix is
unique (e.g. does not exist). The keywords will be appended,
separated by spaces, followed by the extension."
  (let ((n 0))
    (while (directory-files directory nil (format "^%s.*" (mg/note-file-name-make-prefix n)))
      (setq n (+ 1 n)))
    (concat directory "/" (mg/note-file-name-make-prefix n)
            " " (mapconcat 'identity keywords " ") extension)))

(provide 'mg-note-file-name)

;;; mg-note-file-name.el ends here
