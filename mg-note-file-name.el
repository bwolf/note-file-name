;;; mg-note-file-name.el --- Small library for dealing with note file names -*- lexical-binding: t -*-
;;; Commentary:
;;;    See the function documentations.
;;; TODO:
;;;   - downcase keywords
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
  (concat (format-time-string "%Y%m%d")
          (mg/note-file-name-toalphabet n)))

(defun mg/note-sanitize-keyword-string (keyword-string)
  "Create a clean string of `KEYWORD-STRING' with multiple whitespaces removed.
Keywords are separated by \" \"."
  (mapconcat 'identity
             (delq nil (mapcar (lambda (s)
                                 (and (not (string= "" s)) s))
                               (split-string keyword-string "[[:space:]]")))
             " "))

(defun mg/note-file-name-make-unique-file-name (directory extension keyword-string)
  "Make new note file name in DIRECTORY with EXTENSION and KEYWORD-STRING.
The filename will be prefixed with the current date \"%Y%m%d\"
and is followed by a letter suffix which is build with
`mg/note-file-name-toalphabet' such that the date and the letter suffix is
unique (e.g. does not exist). The keywords will be appended,
separated by spaces, followed by the extension."
  (let ((n 0))
    (while (directory-files directory nil (format "^%s.*" (mg/note-file-name-make-prefix n)))
      (setq n (+ 1 n)))
    (concat directory "/" (mg/note-file-name-make-prefix n)
            " " (mg/note-sanitize-keyword-string keyword-string) extension)))

(defvar mg/note-make-markdown-note-directory (directory-file-name (expand-file-name "~/Documents"))
  "Default directory to create markdown notes in.")

(defun mg/note-make-markdown-file (file-name keyword-string)
  "Create the default file contents for given `FILE-NAME' and `KEYWORD-STRING'."
  (with-current-buffer (find-file file-name)
    (insert "# " keyword-string "\n\n")
    (set-buffer-modified-p nil)))

(defun mg/note-make-markdown-note (keyword-string)
  "Create and visit a new markdown note in default directory.
The default directory is defined by `mg/note-make-markdown-note-directory'.
Keywords are queried interactively for the arguments `KEYWORD-STRING'.
Uses internally `mg/note/file-name-make-unique-file-name'.  Queries `KEYWORD-STRING'."
  (interactive "sKeywords: ")
  (let ((file-name (mg/note-file-name-make-unique-file-name mg/note-make-markdown-note-directory ".markdown" (downcase keyword-string))))
    (message "Creating note with file-name %s" file-name)
    (mg/note-make-markdown-file file-name keyword-string)))

(defun mg/note-make-markdown-note-query-directory (dir keyword-string)
  "Create and visit a new markdown note in `DIR' and the given `KEYWORD-STRING'.
Uses internally `mg/note/file-name-make-unique-file-name'.  Queries `DIR' and `KEYWORD-STRING'."
  (interactive "DDirectory: \nsKeywords: ")
  (let ((file-name (mg/note-file-name-make-unique-file-name dir ".markdown" (downcase keyword-string))))
    (message "Creating note with file-name %s" file-name)
    (mg/note-make-markdown-file file-name keyword-string)))

(defun mg/note-file-name-make-unique-file-name1 (directory extension &rest keywords)
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
