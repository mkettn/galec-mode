;;; galec-mode.el --- major mode for editing galec files
;; Copyright (C) 2022       Mark Kettner
;; Copyright (C) 2019       John Tinnerholm
;; Copyright (C) 2019       Free Software Foundation, Inc
;; Copyright (C  2019       Espen Skoglund <esk@gnu.org>
;; Copyright (C) 2010       Dietmar Winkler
;; Copyright (C) 1997--2001 Ruediger Franke
;; Copyright (C) 1997--2001 Free Software Foundation, Inc.

;; Keywords: languages, continuous system modeling
;; Author:   Mark Kettner <mke@offis.de>

;; This code has been written for use with Emacs and shares its licensing.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Based in Modelica mode previously provided by Dietmar Winkler and Ruediger Franke
;; For indention reuses many procedures for Pascal-mode provided by Espen Skoglund
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst galec-mode-version "2.0.1")
;;; History
;;    see ChangeLog
;;; constants
(setq galec-indent-level       2
      galec-case-indent        2
      galec-auto-newline       nil
      galec-tab-always-indent  t
      galec-auto-endcomments   t
      galec-auto-lineup        '(all)
      galec-type-keywords      '("array" "String" "Boolean"
                                    "Integer" "Real" "string" "record" "uniontype")
      galec-start-keywords     '("algorithm" "equation" "function" "method" "end"
                                    "connector" "block" "while" "for" "initial equation"
                                    "initial algorithm" "local"
                                    "match" "matchcontinue"
                                    "reset" "print")
      galec-separator-keywords '("else" "then"))


(defconst galec-class-modifier-keyword
  "\\(encapsulated\\|final\\|inner\\|outer\\|partial\\|re\\(declare\\|placeable\\)\\)[ \t\n\r]+"
  "*Keyword regexp optionally found before a class keyword.")

(defconst galec-class-keyword
  "\\(block\\|c\\(lass\\|onnector\\)\\|function\\|model\\|package\\|record\\|uniontype\\|type\\)[ \t\n\r]+"
  "*Keyword regexp preceding a Galec class declaration or definition.")


(defvar galec-keywords
  '("algorithm" "and" "annotation" "block" "break" "class" "connect" "connector" "constant" "constrainedby"
    "der" "discrete" "each" "else" "elseif" "elsewhen" "encapsulated" "end" "enumeration" "equation"
    "expandable" "extends" "external" "false" "final" "flow" "for" "function" "if" "import" "impure" "in" "initial"
    "inner" "input" "loop" "model" "not" "operator" "or" "outer" "output" "package" "parameter" "partial"
    "protected" "public" "pure" "record" "redeclare" "replaceable" "return" "stream" "then" "true" "type" "when"
    "while" "within"
    ;;The following keywords are for the MetaModelica extension
    "match" "matchcontinue" "list" "array" "as" "case" "then" "local" "record" "uniontype"
    ;; The following keywords are for Galec
    "signals" "method"
    ))

;;;
;;; Regular expressions used to calculate indent, etc.
;;;
;;For completion help
(defconst galec-symbol-re      "\\<[a-zA-Z_][a-zA-Z_0-9.]*\\>")
;Things that starts blocks
(defconst galec-beg-block-re   "\\<\\(\\|model\\|connector\\|record\\|uniontype\\||algorithm\\|equation\\|case\\|record\\|uniontype\\|local\\|function\\|method\\|match\\|matchcontinue\\connector\\|class\\|block\\)\\>")
(defconst galec-end-block-re   "\\<\\(end\\)\\>")
;;Should not be indented 
(defconst galec-declaration-re "\\<\\(constant\\|connector\\|class\\|block\\)\\>")
;;Packages are special
(defconst galec-progbeg-re     "\\<package\\>")
;;Treat functions in their own way :)
(defconst galec-defun-re       "\\<\\(function\\|method\\|class\\package|\\uniontype|record\\block|connector\\)\\>")
;;Things to treat as subblocks
(defconst galec-sub-block-re   "\\<\\(if\\|else\\|for\\|while\\|when\\|case\\)\\>")
;;Things that shall have no extra indention. algorithm,  equation initial
(defconst galec-noindent-re    "\\<\\(algorithm\\|end\\|public\\|protected\\|else if\\|else\\|equation\\|initial\\)\\>")
;;Things that start a new statement
(defconst galec-nosemi-re      "\\<\\(begin\\|then\\|loop\\|else\\)\\>")
;For special functionality
(defconst galec-autoindent-lines-re "\\<\\(import\\)\\>")

;;; Interface to font-lock
(defvar galec-font-lock-keywords nil
  "Keywords to highlight for Galec. See variable `font-lock-keywords'.")

(if galec-font-lock-keywords
    ()
  (setq galec-font-lock-keywords
	(list
	 (list (concat "\\<"
                       (regexp-opt
                        '("import" "within" "extends"
                          "for" "while" "in" "loop" "when"
                          "if" "then" "else" "elseif" "elsewhen"
                          "and" "not" "or" "case"))"\\>")
               0 'font-lock-keyword-face)
	 (list (concat "\\<"
                       (regexp-opt
                        '("algorithm" "equation" "public" "protected" "match" "matchcontinue" "local" "guard" "as") t)
                       "\\>")
               0 'font-lock-keyword-face)
	 (list (concat "\\<"
                       (regexp-opt
                        '("redeclare" "final" "partial" "replaceable"
                          "inner" "outer" "encapsulated"
                          "discrete" "parameter" "constant"
                          "flow" "input" "output" "external"
                          "block" "class" "connector" "function" "model"
                          "package" "record" "type"
                          "end" "uniontype") t) "\\>")
               0 'font-lock-type-face)
	 (list (concat "\\<"
                                        ;(regexp-opt
                                        ; '("der" "analysisType" "initial" "terminal"
                                        ;   "noEvent" "samle" "pre" "edge" "change"
                                        ;   "reinit" "abs" "sign" "sqrt" "div" "mod"
                                        ;   "rem" "ceil" "floor" "integer" "delay"
                                        ;   "cardinality"
                                        ;   "promote" "ndims" "size" "scalar" "vector" "matrix"
                                        ;   "transpose" "outerProduct" "identity" "diagonal"
                                        ;   "zeros" "ones" "fill" "linspace" "min" "max" "sum"
                                        ;   "product" "symmetric" "cross" "skew"
                                        ;) t)
                       "\\(a\\(bs\\|nalysisType\\)\\|c\\(ardinality\\|eil\\|"
                       "hange\\|ross\\)\\|d\\(e\\(lay\\|r\\)\\|i\\(agonal\\|"
                       "v\\)\\)\\|edge\\|f\\(ill\\|loor\\)\\|i\\(dentity\\|"
                       "n\\(itial\\|teger\\)\\)\\|linspace\\|m\\(a\\(trix\\|"
                       "x\\)\\|in\\|od\\)\\|n\\(dims\\|oEvent\\)\\|o\\(nes\\|"
                       "uterProduct\\)\\|pr\\(e\\|o\\(duct\\|mote\\)\\)\\|"
                       "re\\(init\\|m\\)\\|s\\(amle\\|calar\\|i\\(gn\\|"
                       "ze\\)\\|kew\\|qrt\\|um\\|ymmetric\\)\\|t\\(erminal\\|"
                       "ranspose\\)\\|vector\\|zeros\\)"
                       "\\>")
               0 'font-lock-function-name-face)
	 (list (concat "\\<"
                                        ;(regexp-opt
                                        ; '("assert" "terminate") t)
                       "\\(assert\\|terminate\\)"
                       "\\>")
               0 'font-lock-warning-face)
	 (list (concat "\\<"
                                        ;(regexp-opt
                                        ; '("annotation" "connect") t)
                       "\\(annotation\\|connect\\)"
                       "\\>")
               0 (if (string-match "XEmacs" (emacs-version))
                     ;; XEmacs 21.1 still uses old font-lock version
                     (identity 'font-lock-preprocessor-face)
		   (identity 'font-lock-builtin-face)))
	 (list (concat "\\<"
                                        ;(regexp-opt
                                        ; '("false" "true") t)
                       "\\(false\\|true\\)"
                       "\\>")
               0 )
	 (list (concat "\\<"
                                        ;(regexp-opt
                                        ; '("time") t)
                       "\\(time\\)"
                       "\\>")
               0 'font-lock-variable-name-face)))

;;; The mode
  (defvar galec-basic-offset 2
    "*basic offset for indentation in Galec Mode")
  (defvar galec-comment-offset 2
    "*offset for indentation in comments in Galec Mode")
  (defvar galec-statement-offset 2
    "*offset for indentation in statements in Galec Mode")
  (defvar galec-mode-syntax-table nil
    "Syntax table used while in Galec mode.")
  (defvar galec-mode-abbrev-table nil
    "Abbrev table used while in Galec mode.")
  (define-abbrev-table 'galec-mode-abbrev-table ())


  (defun galec-statement-start (&optional ref-point)
    "Move point to the first character of the current statement;
   optional argument points to the last end or unended begin"
    (let ((save-point (point)))
      (if ref-point
          ()
        (condition-case nil
            (galec-last-unended-begin t)
          (error (goto-char (point-min))))
        (setq ref-point (point))
        (goto-char save-point))
      (while (progn
               (re-search-backward
                ;; ("]" ")" ";"
                ;;  "algorithm" "equation" "external"
                ;;  "else" "elseif" "elsewhen"
                ;;  "loop" "protected" "public" "then")
                (concat
                 "[\]\);]\\|"
                 "\\<"
                 "\\(algorithm\\|e\\(lse\\(if\\|when\\)?\\|quation\\|"
                 "xternal\\)\\|loop\\|p\\(rotected\\|ublic\\)\\|then\\)"
                 "\\>")
                ref-point 'no-error)
               (and
                (> (point) ref-point)
                (or (galec-within-comment t)
                    (galec-within-string)
                    (if (looking-at "[\]\)]")
                        (progn
                          (forward-char 1)
                          (forward-sexp -1)
                          t))
                    (if (looking-at ";")
                        (galec-within-matrix-expression t)
                      (galec-within-equation t))))))
      (cond
       ((= (point) ref-point)
        ;; we arrived at last unended begin,
        ;; but might be looking for first statement of block
        (mdc-forward-begin)
        (forward-comment (- (buffer-size)))
        (if (> (point) save-point)
            (goto-char ref-point)))
       ((looking-at ";")
        (forward-char 1))
       (t
        (forward-word 1)))
      (forward-comment (buffer-size))))
 
  (if galec-mode-syntax-table
      ()              ; Do not change the table if it is already set up.
    (setq galec-mode-syntax-table (make-syntax-table))

    (modify-syntax-entry ?_ "w"       galec-mode-syntax-table)
    (modify-syntax-entry ?. "w"       galec-mode-syntax-table)
    (if (string-match "XEmacs" (emacs-version))
        (modify-syntax-entry ?/  ". 1456" galec-mode-syntax-table)
      (modify-syntax-entry ?/  ". 124b" galec-mode-syntax-table))

    (modify-syntax-entry ?*  ". 23"   galec-mode-syntax-table)
    (modify-syntax-entry ?\n "> b"    galec-mode-syntax-table))


;;;
;;;  Macros
;;;
  (defun galec-declaration-end ()
    (let ((nest 1))
      (while (and (> nest 0)
                  (re-search-forward
                   "[:=]\\|\\(\\<record\\>\\)\\|\\(\\<end\\>\\)"
                   (point-at-eol 2) t))
        (cond ((match-beginning 1) (setq nest (1+ nest)))
              ((match-beginning 2) (setq nest (1- nest)))
              ((looking-at "[^(\n]+)") (setq nest 0))))))


  (defun galec-declaration-beg ()
    (let ((nest 1))
      (while (and (> nest 0)
                  (re-search-backward "[:=]\\|\\<\\(type\\|var\\|label\\|const\\)\\>\\|\\(\\<record\\>\\)\\|\\(\\<end\\>\\)" (point-at-bol 0) t))
        (cond ((match-beginning 1) (setq nest 0))
              ((match-beginning 2) (setq nest (1- nest)))
              ((match-beginning 3) (setq nest (1+ nest)))))
      (= nest 0)))


  (defsubst galec-within-string ()
    (nth 3 (parse-partial-sexp (point-at-bol) (point))))


;;;###autoload
  (define-derived-mode galec-mode prog-mode "Galec"
    "Major mode for editing Galec code.\\<galec-mode-map>
TAB indents for Galec code.  Delete converts tabs to spaces as it moves back.
\\[completion-at-point] completes the word around current point with respect \
to position in code
\\[completion-help-at-point] shows all possible completions at this point.
Other useful functions are:
\\[galec-mark-defun]\t- Mark function.
\\[galec-star-comment]\t- insert (* ... *)
\\[galec-comment-area]\t- Put marked area in a comment, fixing nested comments.
\\[galec-uncomment-area]\t- Uncomment an area commented with \
\\[galec-comment-area].
\\[galec-beg-of-defun]\t- Move to beginning of current function.
\\[galec-end-of-defun]\t- Move to end of current function.
\\[galec-goto-class]\t- Goto class prompted for in the minibuffer.
\\[galec-outline-mode]\t- Enter `galec-outline-mode'.
Variables controlling indentation/edit style:
 `galec-indent-level' (default 2)
    Indentation of Galec statements with respect to containing block.
 `galec-case-indent' (default 2)
    Indentation for case statements.
 `galec-auto-newline' (default nil)
    Non-nil means automatically newline after semicolons and the punctuation
    mark after an end.
 `galec-indent-nested-classes' (default t)
    Non-nil means nested functions are indented.
 `galec-tab-always-indent' (default t)
    Non-nil means TAB in Galec mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 `galec-auto-endcomments' (default t)
    Non-nil means a comment { ... } is set after the ends which ends cases and
    functions. The name of the function or case will be set between the braces.
 `galec-auto-lineup' (default t)
    List of contexts where auto lineup of :'s or ='s should be done.
See also the user variables `galec-type-keywords', `galec-start-keywords' and
`galec-separator-keywords'."

    `;;For comments
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-multi-line)
  (setq
   comment-column 32
   comment-start "// "
   comment-start-skip "//*"
   comment-end ""
   comment-multi-line nil)
  

    (setq-local local-abbrev-table galec-mode-abbrev-table)
    (setq-local indent-line-function 'galec-indent-line)
    (setq-local comment-indent-function 'galec-indent-comment)
    (setq-local parse-sexp-ignore-comments nil)
    (setq-local blink-matching-paren-dont-ignore-comments t)
    (setq-local case-fold-search t)
    (add-hook 'completion-at-point-functions 'galec-completions-at-point nil t)
    ;; Font lock support
    (setq-local font-lock-defaults '(galec-font-lock-keywords nil nil))
    (setq-local imenu-case-fold-search t)
    ;; Galec-mode's own hide/show support.
    (add-to-invisibility-spec '(galec . t)))

;;;
;;;  Electric functions
;;;
  (defun electric-galec-terminate-line ()
    "Terminate line and indent next line."
    (interactive)
    ;; First, check if current line should be indented
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (if (looking-at galec-autoindent-lines-re)
          (galec-indent-line)))
    (delete-horizontal-space) ; Removes trailing whitespaces
    (newline)
    ;; Indent next line
    (galec-indent-line)
    ;; Check if we shall indent inside comment
    (let ((setstar nil))
      (save-excursion
        (forward-line -1)
        (skip-chars-forward " \t")
        (cond ((looking-at "\\*[ \t]+)")
               ;; Delete region between `*' and `)' if there is only whitespaces.
               (forward-char 1)
               (delete-horizontal-space))
              ((and (looking-at "(\\*\\|\\*[^)]")
                    (not (save-excursion (search-forward "*)" (point-at-eol) t))))
               (setq setstar t))))
      ;; If last line was a star comment line then this one shall be too.
      (if (null setstar)
          (galec-indent-line)
        (insert "*  "))))


  (defun electric-galec-semi-or-dot ()
    "Insert `;' or `.' character and reindent the line."
    (interactive)
    (insert last-command-event)
    (save-excursion
      (beginning-of-line)
      (galec-indent-line))
    (if galec-auto-newline
        (electric-galec-terminate-line)))

  (defun electric-galec-colon ()
    "Insert `:' and do all indentations except line indent on this line."
    (interactive)
    (insert last-command-event)
    ;; Do nothing if within string.
    (if (galec-within-string)
        ()
      (save-excursion
        (beginning-of-line)
        (galec-indent-line))
      (let ((galec-tab-always-indent nil))
        (galec-indent-command))))

  (defun electric-galec-equal ()
    "Insert `=', and do indentation if within type declaration."
    (interactive)
    (insert last-command-event)
    (if (eq (car (galec-calculate-indent)) 'declaration)
        (let ((galec-tab-always-indent nil))
          (galec-indent-command))))

  (defun electric-galec-hash ()
    "Insert `#', and indent to column 0 if this is a CPP directive."
    (interactive)
    (insert last-command-event)
    (if (save-excursion (beginning-of-line) (looking-at "^[ \t]*#"))
        (save-excursion (beginning-of-line)
                        (delete-horizontal-space))))

  (defun electric-galec-tab ()
    "Function called when TAB is pressed in Galec mode."
    (interactive)
    ;; Do nothing if within a string or in a CPP directive.
    (if (or (galec-within-string)
            (and (not (bolp))
                 (save-excursion (beginning-of-line) (eq (following-char) ?#))))
        (insert "\t")
      ;; If galec-tab-always-indent, indent the beginning of the line.
      (if galec-tab-always-indent
          (save-excursion
            (beginning-of-line)
            (galec-indent-line))
        (if (save-excursion
              (skip-chars-backward " \t")
              (bolp))
            (galec-indent-line)
          (insert "\t")))
      (galec-indent-command)))
;;;
;;; Interactive functions
;;;
  (defvar galec--extra-indent 0)

  (defun galec-insert-block ()
    "Insert Galec begin ... end; block in the code with right indentation."
    (interactive)
    (insert "begin")
    (electric-galec-terminate-line)
    (save-excursion
      (newline)
      (insert "end")
      (beginning-of-line)
      (galec-indent-line)))

  (defun galec-star-comment ()
    "Insert Galec star comment at point."
    (interactive)
    (galec-indent-line)
    (insert "/*")
    (electric-galec-terminate-line)
    (save-excursion
      (electric-galec-terminate-line)
      (delete-horizontal-space)
      (insert "*/"))
    (insert "  "))

  (defun galec-mark-defun ()
    "Mark the current Galec function (or procedure).
This puts the mark at the end, and point at the beginning."
    (interactive)
    (push-mark)
    (galec-end-of-defun)
    (push-mark)
    (galec-beg-of-defun))

  (defun galec-comment-area (start end))

  (defun galec-uncomment-area ())

  (defun galec-beg-of-defun ()
    "Move backward to the beginning of the current function or procedure."
    (interactive)
    (catch 'found
      (if (not (looking-at (concat "\\s \\|\\s)\\|" galec-defun-re)))
          (forward-sexp 1))
      (let ((nest 0) (max -1) (func 0)
            (reg (concat galec-beg-block-re "\\|"
                         galec-end-block-re "\\|"
                         galec-defun-re)))
        (while (re-search-backward reg nil 'move)
          (cond ((let ((state (save-excursion
                                (parse-partial-sexp (point-min) (point)))))
                   (or (nth 3 state) (nth 4 state))) ; Inside string or comment
                 ())
                ((match-end 1)                       ; begin|case|record|repeat
                 (if (and (looking-at "\\<record\\>") (>= max 0))
                     (setq func (1- func)))
                 (setq nest (1+ nest)
                       max (max nest max)))
                ((match-end 2)                       ; end|until
                 (if (and (= nest max) (>= max 0))
                     (setq func (1+ func)))
                 (setq nest (1- nest)))
                ((match-end 3)                       ; function|procedure
                 (if (= 0 func)
                     (throw 'found t)
                   (setq func (1- func)))))))
      nil))

  (defun galec-end-of-defun ()
    "Move forward to the end of the current function or procedure."
    (interactive)
    (if (looking-at "\\s ")
        (forward-sexp 1))
    (if (not (looking-at galec-defun-re))
        (galec-beg-of-defun))
    (forward-char 1)
    (let ((nest 0) (func 1)
          (reg (concat galec-beg-block-re "\\|"
                       galec-end-block-re "\\|"
                       galec-defun-re)))
      (while (and (/= func 0)
                  (re-search-forward reg nil 'move))
        (cond ((let ((state (save-excursion
			      (parse-partial-sexp (point-min) (point)))))
		 (or (nth 3 state) (nth 4 state))) ; Inside string or comment
	       ())
              ((match-end 1)
               (setq nest (1+ nest))
               (if (save-excursion
                     (goto-char (match-beginning 0))
                     (looking-at "\\<record\\>"))
                   (setq func (1+ func))))
              ((match-end 2)
               (setq nest (1- nest))
               (if (= nest 0)
                   (setq func (1- func))))
              ((match-end 3)
               (setq func (1+ func))))))
    (forward-line 1))

  (defun galec-end-of-statement ()
    "Move forward to end of current statement."
    (interactive)
    (let ((parse-sexp-ignore-comments t)
          (nest 0) pos
          (regexp (concat "\\(" galec-beg-block-re "\\)\\|\\("
                          galec-end-block-re "\\)")))
      (if (not (looking-at "[ \t\n]")) (forward-sexp -1))
      (or (looking-at galec-beg-block-re)
          ;; Skip to end of statement
          (setq pos (catch 'found
                      (while t
                        (forward-sexp 1)
                        (cond ((looking-at "[ \t]*;")
                               (skip-chars-forward "^;")
                               (forward-char 1)
                               (throw 'found (point)))
                              ((save-excursion
                                 (forward-sexp -1)
                                 (looking-at galec-beg-block-re))
                               (goto-char (match-beginning 0))
                               (throw 'found nil))
                              ((eobp)
                               (throw 'found (point))))))))
      (if (not pos)
          ;; Skip a whole block
          (catch 'found
            (while t
              (re-search-forward regexp nil 'move)
              (setq nest (if (match-end 1)
                             (1+ nest)
                           (1- nest)))
              (cond ((eobp)
                     (throw 'found (point)))
                    ((= 0 nest)
                     (throw 'found (galec-end-of-statement))))))
        pos)))

  (defun galec-downcase-keywords ()
    "Downcase all Galec keywords in the buffer."
    (interactive)
    (galec-change-keywords 'downcase-word))

  (defun galec-upcase-keywords ()
    "Upcase all Galec keywords in the buffer."
    (interactive)
    (galec-change-keywords 'upcase-word))

  (defun galec-capitalize-keywords ()
    "Capitalize all Galec keywords in the buffer."
    (interactive)
    (galec-change-keywords 'capitalize-word))

  ;; Change the keywords according to argument.
  (defun galec-change-keywords (change-word)
    (save-excursion
      (let ((keyword-re (concat "\\<\\("
                                (mapconcat 'identity galec-keywords "\\|")
                                "\\)\\>")))
        (goto-char (point-min))
        (while (re-search-forward keyword-re nil t)
          (funcall change-word -1))))))

;;;
;;; Indentation
;;;
(defconst galec-indent-alist
  '((block . (+ galec--extra-indent galec-indent-level))
    (case . (+ galec--extra-indent galec-case-indent))
    (caseblock . galec--extra-indent) (cpp . 0)
    (declaration . (+ galec--extra-indent galec-indent-level))
    (paramlist . (galec-indent-paramlist t))
    (comment . (galec-indent-comment))
    (defun . galec--extra-indent) (contexp . galec--extra-indent)
    (unknown . galec--extra-indent) (string . 0) (progbeg . 0)))

(defun galec-indent-command ()
  "Indent for special part of code."
  (let* ((indent-str (galec-calculate-indent))
	 (type (car indent-str)))
    (cond ((and (eq type 'paramlist)
		(or (memq 'all galec-auto-lineup)
		    (memq 'paramlist galec-auto-lineup)))
	   (galec-indent-paramlist)
	   (galec-indent-paramlist))
	  ((and (eq type 'declaration)
		(or (memq 'all galec-auto-lineup)
		    (memq 'declaration  galec-auto-lineup)))
	   (galec-indent-declaration))
	  ((and (eq type 'case) (not (looking-at "^[ \t]*$"))
		(or (memq 'all galec-auto-lineup)
		    (memq 'case galec-auto-lineup)))
	   (galec-indent-case)))
    (if (looking-at "[ \t]+$")
	(skip-chars-forward " \t"))))

(defun galec-indent-line ()
  "Indent current line as a Galec statement."
  (let* ((indent-str (galec-calculate-indent))
	 (type (car indent-str))
	 (galec--extra-indent (car (cdr indent-str))))
    ;; Labels should not be indented.
    (if (and (looking-at "^[0-9a-zA-Z]+[ \t]*:[^=]")
	     (not (eq type 'declaration)))
	(search-forward ":" nil t))
    (delete-horizontal-space)
    (cond (; Some things should not be indented
	   (or (and (eq type 'declaration) (looking-at galec-declaration-re))
	       (eq type 'cpp))
	   ())
	  (; Other things should have no extra indent
	   (looking-at galec-noindent-re)
	   (indent-to galec--extra-indent))
	  (; Nested functions should be indented
	   (looking-at galec-defun-re)
	   (if (and galec-indent-nested-classes
		    (eq type 'defun))
	       (indent-to (+ galec--extra-indent galec-indent-level))
	     (indent-to galec--extra-indent)))
	  (; But most lines are treated this way
	   (indent-to (eval (cdr (assoc type galec-indent-alist))))
	   ))))

(defun galec-calculate-indent ()
  "Calculate the indent of the current Galec line.
Return a list of two elements: (INDENT-TYPE INDENT-LEVEL)."
  (save-excursion
    (let* ((parse-sexp-ignore-comments t)
	   (oldpos (point))
	   (state (save-excursion (parse-partial-sexp (point-min) (point))))
	   (nest 0) (par 0) (complete (looking-at "[ \t]*end\\>"))
	   (elsed (looking-at "[ \t]*else\\>")) (funccnt 0)
	   (did-func (looking-at "[ \t]*\\(procedure\\|function\\)\\>"))
	   (type (catch 'nesting
		   ;; Check if inside a string, comment or parenthesis
		   (cond ((nth 3 state) (throw 'nesting 'string))
			 ((nth 4 state) (throw 'nesting 'comment))
			 ((> (car state) 0)
			  (goto-char (scan-lists (point) -1 (car state)))
			  (setq par (1+ (current-column))))
			 ((save-excursion (beginning-of-line)
					  (eq (following-char) ?#))
			  (throw 'nesting 'cpp)))
		   ;; Loop until correct indent is found
		   (while t
		     (backward-sexp 1)
		     (cond (;--Escape from case statements
			    (and (looking-at "[A-Za-z0-9]+[ \t]*:[^=]")
				 (not complete)
				 (save-excursion (skip-chars-backward " \t")
						 (bolp))
				 (= (save-excursion
				      (end-of-line) (backward-sexp) (point))
				    (point))
				 (> (save-excursion (goto-char oldpos)
						    (beginning-of-line)
						    (point))
				    (point)))
			    (throw 'nesting 'caseblock))
			   (;--Beginning of program
			    (looking-at galec-progbeg-re)
			    (throw 'nesting 'progbeg))
			   (;--No known statements
			    (bobp)
			    (throw 'nesting 'progbeg))
			   (;--Nest block outwards
			    (looking-at galec-beg-block-re)
			    (if (= nest 0)
				(cond ((looking-at "case\\>")
				       (throw 'nesting 'case))
				      ((looking-at "record\\>")
				       (throw 'nesting 'declaration))
				      (t (throw 'nesting 'block)))
			      (if (and (looking-at "record\\>") (= nest 1))
				  (setq funccnt (1- funccnt)))
			      (setq nest (1- nest))))
			   (;--Nest block inwards
			    (looking-at galec-end-block-re)
			    (if (and (looking-at "end\\s ")
				     elsed (not complete))
				(throw 'nesting 'block))
			    (if (= nest 0)
				(setq funccnt (1+ funccnt)))
			    (setq complete t
				  nest (1+ nest)))
			   (;--Defun (or parameter list)
			    (and (looking-at galec-defun-re)
				 (progn (setq funccnt (1- funccnt)
					      did-func t)
					(or (bolp) (< funccnt 0))))
			    ;; Prevent searching whole buffer
			    (if (and (bolp) (>= funccnt 0))
				(throw 'nesting 'progbeg))
			    (if (= 0 par)
				(throw 'nesting 'defun)
			      (setq par 0)
			      (let ((n 0))
				(while (re-search-forward
					"\\(\\<record\\>\\)\\|\\<end\\>"
					oldpos t)
				  (if (match-end 1)
				      (setq n (1+ n)) (setq n (1- n))))
				(if (> n 0)
				    (throw 'nesting 'declaration)
				  (throw 'nesting 'paramlist)))))
			   (;--Declaration part
			    (and (looking-at galec-declaration-re)
				 (not did-func)
				 (= funccnt 0))
			    (if (save-excursion
				  (goto-char oldpos)
				  (forward-line -1)
				  (looking-at "^[ \t]*$"))
				(throw 'nesting 'unknown)
			      (throw 'nesting 'declaration)))
			   (;--If, else or while statement
			    (and (not complete)
				 (looking-at galec-sub-block-re))
			    (throw 'nesting 'block))
			   (;--Found complete statement
			    (save-excursion (forward-sexp 1)
					    (= (following-char) ?\;))
			    (setq complete t))
			   )))))

      ;; Return type of block and indent level.
      (if (> par 0)                               ; Unclosed Parenthesis
	  (list 'contexp par)
	(list type (galec-indent-level))))))

(defun galec-indent-level ()
  "Return the indent-level the current statement has.
Do not count labels, case statements or records."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "[ \t]*[0-9a-zA-Z]+[ \t]*:[^=]")
	(search-forward ":" nil t)
      (if (looking-at ".*=[ \t]*record\\>")
	  (search-forward "=" nil t)))
    (skip-chars-forward " \t")
    (current-column)))

(defun galec-indent-comment ()
  "Return indent for current comment."
  (save-excursion
    (re-search-backward "\\((\\*\\)\\|{" nil t)
    (if (match-beginning 1)
	(1+ (current-column))
      (current-column))))

(defun galec-indent-case ()
  "Indent within case statements."
  (let ((savepos (point-marker))
	(end (prog2
		 (end-of-line)
		 (point-marker)
	       (re-search-backward "\\<case\\>" nil t)))
	(beg (point))
	(galec--extra-indent 0))
    ;; Get right indent
    (while (< (point) end)
      (if (re-search-forward
	   "^[ \t]*[^ \t,:]+[ \t]*\\(,[ \t]*[^ \t,:]+[ \t]*\\)*:"
	   (marker-position end) 'move)
	  (forward-char -1))
      (if (< (point) end)
	  (progn
	    (delete-horizontal-space)
	    (if (> (current-column) galec--extra-indent)
		(setq galec--extra-indent (current-column)))
	    (galec-end-of-statement))))
    (goto-char beg)
    ;; Indent all case statements
    (while (< (point) end)
      (if (re-search-forward
	   "^[ \t]*[^][ \t,\\.:]+[ \t]*\\(,[ \t]*[^ \t,:]+[ \t]*\\)*:"
	   (marker-position end) 'move)
	  (forward-char -1))
      (indent-to (1+ galec--extra-indent))
      (if (/= (following-char) ?:)
	  ()
	(forward-char 1)
	(delete-horizontal-space)
	(insert " "))
      (galec-end-of-statement))
    (goto-char savepos)))

(defun galec-indent-paramlist (&optional arg)
  "Indent current line in parameterlist.
If optional ARG is non-nil, just return the
indent of the current line in parameterlist."
  (save-excursion
    (let* ((oldpos (point))
	   (stpos (progn (goto-char (scan-lists (point) -1 1)) (point)))
	   (stcol (1+ (current-column)))
	   (edpos (progn (galec-declaration-end)
			 (search-backward ")" (point-at-bol) t)
			 (point)))
	   (usevar (re-search-backward "\\<var\\>" stpos t)))
      (if arg (progn
		;; If arg, just return indent
		(goto-char oldpos)
		(beginning-of-line)
		(if (or (not usevar) (looking-at "[ \t]*var\\>"))
		    stcol (+ 4 stcol)))
	(goto-char stpos)
	(forward-char 1)
	(delete-horizontal-space)
	(if (and usevar (not (looking-at "var\\>")))
	    (indent-to (+ 4 stcol)))
	(galec-indent-declaration nil stpos edpos)))))

(defun galec-indent-declaration (&optional arg start end)
  "Indent current lines as declaration, lining up the `:'s or `='s."
  (let ((pos (point-marker)))
    (if (and (not (or arg start)) (not (galec-declaration-beg)))
	()
      (let ((lineup (if (or (looking-at "\\<var\\>\\|\\<record\\>") arg start)
			":" "="))
	    (stpos (if start start
                     (forward-word-strictly 2) (backward-word 1) (point)))
	    (edpos (set-marker (make-marker)
			       (if end end
				 (max (progn (galec-declaration-end)
					     (point))
				      pos))))
	    galec--extra-indent)

	(goto-char stpos)
	;; Indent lines in record block
	(if arg
	    (while (<= (point) edpos)
	      (beginning-of-line)
	      (delete-horizontal-space)
	      (if (looking-at "end\\>")
		  (indent-to arg)
		(indent-to (+ arg galec-indent-level)))
	      (forward-line 1)))

	;; Do lineup
	(setq galec--extra-indent (galec-get-lineup-indent stpos edpos lineup))
	(goto-char stpos)
	(while (and (<= (point) edpos) (not (eobp)))
	  (if (search-forward lineup (point-at-eol) 'move)
	      (forward-char -1))
	  (delete-horizontal-space)
	  (indent-to galec--extra-indent)
	  (if (not (looking-at lineup))
	      (forward-line 1) ; No more indent if there is no : or =
	    (forward-char 1)
	    (delete-horizontal-space)
	    (insert " ")
	    ;; Indent record block
	    (if (looking-at "record\\>")
		(galec-indent-declaration (current-column)))
	    (forward-line 1)))))

    ;; If arg - move point
    (if arg (forward-line -1)
      (goto-char pos))))

                                        ;  "Return the indent level that will line up several lines within the region
                                        ;from b to e nicely. The lineup string is str."
(defun galec-get-lineup-indent (b e str)
  (save-excursion
    (let ((galec--extra-indent 0)
	  (reg (concat str "\\|\\(\\<record\\>\\)\\|" galec-defun-re)))
      (goto-char b)
      ;; Get rightmost position
      (while (< (point) e)
	(and (re-search-forward reg (min e (point-at-eol 2)) 'move)
	     (cond ((match-beginning 1)
		    ;; Skip record blocks
		    (galec-declaration-end))
		   ((match-beginning 2)
		    ;; We have entered a new procedure.  Exit.
		    (goto-char e))
		   (t
		    (goto-char (match-beginning 0))
		    (skip-chars-backward " \t")
		    (if (> (current-column) galec--extra-indent)
			(setq galec--extra-indent (current-column)))
		    (goto-char (match-end 0))
		    (end-of-line)
		    ))))
      ;; In case no lineup was found
      (if (> galec--extra-indent 0)
	  (1+ galec--extra-indent)
	;; No lineup-string found
	(goto-char b)
	(end-of-line)
	(skip-chars-backward " \t")
	(1+ (current-column))))))

;;;
;;; Completion
(defun galec-string-diff (str1 str2)
  "Return index of first letter where STR1 and STR2 differs."
  (catch 'done
    (let ((diff 0))
      (while t
	(if (or (> (1+ diff) (length str1))
		(> (1+ diff) (length str2)))
	    (throw 'done diff))
	(or (equal (aref str1 diff) (aref str2 diff))
	    (throw 'done diff))
	(setq diff (1+ diff))))))

;; Calculate all possible completions for functions if argument is `function',
;; completions for procedures if argument is `procedure' or both functions and
;; procedures otherwise.

(defun galec-func-completion (type galec-str)
  ;; Build regular expression for function/procedure names
  (save-excursion
    (if (string= galec-str "")
        (setq galec-str "[a-zA-Z_]"))
    (let ((galec-str (concat (cond
                                 ((eq type 'procedure) "\\<\\(procedure\\)\\s +")
                                 ((eq type 'function) "\\<\\(function\\)\\s +")
                                 (t "\\<\\(function\\|procedure\\)\\s +"))
                                "\\<\\(" galec-str "[a-zA-Z0-9_.]*\\)\\>"))
          (galec-all ())
          match)

      (if (not (looking-at "\\<\\(function\\|procedure\\)\\>"))
          (re-search-backward "\\<\\(function\\|procedure\\)\\>" nil t))
      (forward-char 1)

      ;; Search through all reachable functions
      (while (galec-beg-of-defun)
        (if (re-search-forward galec-str (point-at-eol) t)
            (progn (setq match (buffer-substring (match-beginning 2)
                                                 (match-end 2)))
                   (push match galec-all)))
        (goto-char (match-beginning 0)))

      galec-all)))

(defun galec-get-completion-decl (galec-str)
  ;; Macro for searching through current declaration (var, type or const)
  ;; for matches of `str' and adding the occurrence to `all'
  (let ((end (save-excursion (galec-declaration-end)
			     (point)))
        (galec-all ())
	match)
    ;; Traverse lines
    (while (< (point) end)
      (if (re-search-forward "[:=]" (point-at-eol) t)
	  ;; Traverse current line
	  (while (and (re-search-backward
		       (concat "\\((\\|\\<\\(var\\|type\\|const\\)\\>\\)\\|"
			       galec-symbol-re)
		       (point-at-bol) t)
		      (not (match-end 1)))
	    (setq match (buffer-substring (match-beginning 0) (match-end 0)))
	    (if (string-match (concat "\\<" galec-str) match)
                (push match galec-all))))
      (if (re-search-forward "\\<record\\>" (point-at-eol) t)
	  (galec-declaration-end)
	(forward-line 1)))

    galec-all))

(defun galec-type-completion (galec-str)
  "Calculate all possible completions for types."
  (let ((start (point))
        (galec-all ())
	goon)
    ;; Search for all reachable type declarations
    (while (or (galec-beg-of-defun)
	       (setq goon (not goon)))
      (save-excursion
	(if (and (< start (prog1 (save-excursion (galec-end-of-defun)
						 (point))
			    (forward-char 1)))
		 (re-search-forward
		  "\\<type\\>\\|\\<\\(begin\\|function\\)\\>"
		  start t)
		 (not (match-end 1)))
	    ;; Check current type declaration
            (setq galec-all
                  (nconc (galec-get-completion-decl galec-str)
                         galec-all)))))

    galec-all))

(defun galec-var-completion (prefix)
  "Calculate all possible completions for variables (or constants)."
  (save-excursion
    (let ((start (point))
          (galec-all ())
          goon twice)
      ;; Search for all reachable var declarations
      (while (or (galec-beg-of-defun)
                 (setq goon (not goon)))
        (save-excursion
          (if (> start (prog1 (save-excursion (galec-end-of-defun)
                                              (point))))
              ()                        ; Declarations not reachable
            (if (search-forward "(" (point-at-eol) t)
                ;; Check parameterlist
                ;; FIXME: galec-get-completion-decl doesn't understand
                ;; the var declarations in parameter lists :-(
                (setq galec-all
                      (nconc (galec-get-completion-decl prefix)
                             galec-all)))
            (setq twice 2)
            (while (>= (setq twice (1- twice)) 0)
              (cond
               ((and (re-search-forward
                      (concat "\\<\\(var\\|const\\)\\>\\|"
                              "\\<\\(begin\\|function\\|procedure\\)\\>")
                      start t)
                     (not (match-end 2)))
                ;; Check var/const declarations
                (setq galec-all
                      (nconc (galec-get-completion-decl prefix)
                             galec-all)))
               ((match-end 2)
                (setq twice 0)))))))
      galec-all)))


(defun galec-keyword-completion (keyword-list galec-str)
  "Give list of all possible completions of keywords in KEYWORD-LIST."
  (let ((galec-all ()))
    (dolist (s keyword-list)
      (if (string-match (concat "\\<" galec-str) s)
          (push s galec-all)))
    galec-all))

;; Function passed to completing-read, try-completion or
;; all-completions to get completion on STR. If predicate is non-nil,
;; it must be a function to be called for every match to check if this
;; should really be a match. If flag is t, the function returns a list
;; of all possible completions. If it is nil it returns a string, the
;; longest possible completion, or t if STR is an exact match. If flag
;; is 'lambda, the function returns t if STR is an exact match, nil
;; otherwise.

(defvar galec-completion-cache nil)

(defun galec-completion (galec-str galec-pred galec-flag)
  (let ((all (car galec-completion-cache)))
    ;; Check the cache's freshness.
    (unless (and galec-completion-cache
                 (string-prefix-p (nth 1 galec-completion-cache) galec-str)
                 (eq (current-buffer) (nth 2 galec-completion-cache))
                 (eq (field-beginning) (nth 3 galec-completion-cache)))
      (let ((state (car (galec-calculate-indent))))
        (setq all
              ;; Determine what should be completed
              (cond
               (              ;--Within a declaration or parameterlist
                (or (eq state 'declaration) (eq state 'paramlist)
                    (and (eq state 'defun)
                         (save-excursion
                           (re-search-backward ")[ \t]*:" (point-at-bol) t))))
                (if (or (eq state 'paramlist) (eq state 'defun))
                    (galec-beg-of-defun))
                (nconc
                 (galec-type-completion galec-str)
                 (galec-keyword-completion galec-type-keywords galec-str)))
               (                        ;--Starting a new statement
                (and (not (eq state 'contexp))
                     (save-excursion
                       (skip-chars-backward "a-zA-Z0-9_.")
                       (backward-sexp 1)
                       (or (looking-at galec-nosemi-re)
                           (progn
                             (forward-sexp 1)
                             (looking-at "\\s *\\(;\\|:[^=]\\)")))))
                (nconc
                 (galec-var-completion galec-str)
                 (galec-func-completion 'procedure galec-str)
                 (galec-keyword-completion galec-start-keywords galec-str)))
               (t                       ;--Anywhere else
                (nconc
                 (galec-var-completion galec-str)
                 (galec-func-completion 'function galec-str)
                 (galec-keyword-completion galec-separator-keywords
                                              galec-str)))))

        (setq galec-completion-cache
              (list all galec-str (current-buffer) (field-beginning)))))

    ;; Now we have built a list of all matches. Give response to caller
    (complete-with-action galec-flag all galec-str galec-pred)))

(defvar galec-last-word-numb 0)
(defvar galec-last-word-shown nil)
(defvar galec-last-completions nil)

(defun galec-completions-at-point ()
  (let* ((b (save-excursion (skip-chars-backward "a-zA-Z0-9_") (point)))
	 (e (save-excursion (skip-chars-forward "a-zA-Z0-9_") (point))))
    (when (> e b)
      (list b e #'galec-completion))))

(define-obsolete-function-alias 'galec-complete-word
  'completion-at-point "24.1")

(define-obsolete-function-alias 'galec-show-completions
  'completion-help-at-point "24.1")


(defun galec-get-default-symbol ()
  "Return symbol around current point as a string."
  (save-excursion
    (buffer-substring (progn
			(skip-chars-backward " \t")
			(skip-chars-backward "a-zA-Z0-9_")
			(point))
		      (progn
			(skip-chars-forward "a-zA-Z0-9_")
			(point)))))

(defun galec-build-defun-re (str &optional arg)
  "Return function/procedure starting with STR as regular expression.
With optional second arg non-nil, STR is the complete name of the instruction."
  (if arg
      (concat "^\\(function\\|class\\package|\\uniontype|record\\block|connector\\)[ \t]+\\(" str "\\)\\>")
    (concat "^\\(function\\|class\\package|\\uniontype|record\\block|connector\\)[ \t]+\\(" str "[a-zA-Z0-9_]*\\)\\>")))
;; Function passed to completing-read, try-completion or
;; all-completions to get completion on any function name. If
;; predicate is non-nil, it must be a function to be called for every
;; match to check if this should really be a match. If flag is t, the
;; function returns a list of all possible completions. If it is nil
;; it returns a string, the longest possible completion, or t if STR
;; is an exact match. If flag is 'lambda, the function returns t if
;; STR is an exact match, nil otherwise.

(defun galec-comp-defun (galec-str galec-pred galec-flag)
  (save-excursion
    (let ((galec-all nil))

      ;; Build regular expression for functions
      (let ((galec-str (galec-build-defun-re (if (string= galec-str "")
                                                       "[a-zA-Z_]"
                                                     galec-str))))
        (goto-char (point-min))

        ;; Build a list of all possible completions
        (while (re-search-forward galec-str nil t)
          (push (match-string 2) galec-all)))

      ;; Now we have built a list of all matches. Give response to caller
      (complete-with-action galec-flag galec-all galec-str galec-pred))))

(defun galec-goto-class ()
  "Move to specified Galec function/procedure/model/class.
The default is a name found in the buffer around point."
  (interactive)
  (let* ((default (galec-get-default-symbol))
	 (default (if (galec-comp-defun default nil 'lambda)
		      default ""))
	 (label
          ;; Do completion with default.
          (completing-read (if (not (string= default ""))
                               (concat "Label (default " default "): ")
                             "Label: ")
                           ;; Complete with the defuns found in the
                           ;; current-buffer.
                           (let ((buf (current-buffer)))
                             (lambda (s p a)
                               (with-current-buffer buf
                                 (galec-comp-defun s p a))))
                           nil t "")))
    ;; If there was no response on prompt, use default value.
    (if (string= label "")
	(setq label default))
    ;; Goto right place in buffer if label is not an empty string.
    (or (string= label "")
	(progn
	  (goto-char (point-min))
	  (re-search-forward (galec-build-defun-re label t))
	  (beginning-of-line)))))

;;;
;;; Galec-outline-mode
;;;
(defvar galec-outline-map
  (let ((map (make-sparse-keymap)))
    (if (fboundp 'set-keymap-name)
        (set-keymap-name galec-outline-map 'galec-outline-map))
    (define-key map "\M-\C-a"  'galec-outline-prev-defun)
    (define-key map "\M-\C-e"  'galec-outline-next-defun)
    (define-key map "\C-c\C-d" 'galec-outline-goto-defun)
    (define-key map "\C-c\C-s" 'galec-show-all)
    (define-key map "\C-c\C-h" 'galec-hide-other-defuns)
    map)
  "Keymap used in Galec Outline mode.")

(define-minor-mode galec-outline-mode
  "Outline-line minor mode for Galec mode.
When enabled, portions of the text being edited may be made
invisible.\\<galec-outline-map>
Galec Outline mode provides some additional commands.
\\[galec-outline-prev-defun]\
\t- Move to previous function/procedure, hiding everything else.
\\[galec-outline-next-defun]\
\t- Move to next function/procedure, hiding everything else.
\\[galec-outline-goto-defun]\
\t- Goto function/procedure prompted for in minibuffer,
\t  hide all other functions.
\\[galec-show-all]\t- Show the whole buffer.
\\[galec-hide-other-defuns]\
\t- Hide everything but the current function (function under the cursor).
\\[galec-outline-mode]\t- Leave Galec Outline mode."
  :init-value nil :lighter " Outl" :keymap galec-outline-map
  (add-to-invisibility-spec '(galec . t))
  (unless galec-outline-mode
    (galec-show-all)))

(defun galec-outline-change (b e hide)
  (when (> e b)
    ;; We could try and optimize this in the case where the region is
    ;; already hidden.  But I'm not sure it's worth the trouble.
    (remove-overlays b e 'invisible 'galec)
    (when hide
      (let ((ol (make-overlay b e nil t nil)))
        (overlay-put ol 'invisible 'galec)
        (overlay-put ol 'evaporate t)))))

(defun galec-show-all ()
  "Show all of the text in the buffer."
  (interactive)
  (galec-outline-change (point-min) (point-max) nil))

(defun galec-hide-other-defuns ()
  "Show only the current defun."
  (interactive)
  (save-excursion
    (let ((beg (progn (if (not (looking-at "\\(function\\|procedure\\)\\>"))
			  (galec-beg-of-defun))
		      (line-beginning-position)))
	  (end (progn (galec-end-of-defun)
		      (backward-sexp 1)
                      (line-beginning-position 2)))
	  (opoint (point-min)))
      ;; BEG at BOL.
      ;; OPOINT at EOL.
      ;; END at BOL.
      (goto-char (point-min))

      ;; Hide all functions before current function
      (while (re-search-forward "^[ \t]*\\(function\\|procedure\\)\\>"
                                beg 'move)
	(galec-outline-change opoint (line-end-position 0) t)
	(setq opoint (line-end-position))
	;; Functions may be nested
	(if (> (progn (galec-end-of-defun) (point)) beg)
	    (goto-char opoint)))
      (if (> beg opoint)
	  (galec-outline-change opoint (1- beg) t))

      ;; Show current function
      (galec-outline-change (1- beg) end nil)
      ;; Hide nested functions
      (forward-char 1)
      (while (re-search-forward "^\\(function\\|class\\package|\\uniontype|record\\block|connector\\)\\>" end 'move)
	(setq opoint (line-end-position))
	(galec-end-of-defun)
	(galec-outline-change opoint (line-end-position) t))

      (goto-char end)
      (setq opoint end)

      ;; Hide all function after current function
      (while (re-search-forward "^\\(function\\|class\\package|\\uniontype|record\\block|connector\\)\\>" nil 'move)
	(galec-outline-change opoint (line-end-position 0) t)
	(setq opoint (line-end-position))
	(galec-end-of-defun))
      (galec-outline-change opoint (point-max) t)

      ;; Hide main program
      (if (< (progn (forward-line -1) (point)) end)
	  (progn
	    (goto-char beg)
	    (galec-end-of-defun)
	    (backward-sexp 1)
	    (galec-outline-change (line-end-position) (point-max) t))))))

(defun galec-outline-next-defun ()
  "Move to next function/procedure, hiding all others."
  (interactive)
  (galec-end-of-defun)
  (galec-hide-other-defuns))

(defun galec-outline-prev-defun ()
  "Move to previous function/procedure, hiding all others."
  (interactive)
  (galec-beg-of-defun)
  (galec-hide-other-defuns))

(defun galec-outline-goto-defun ()
  "Move to specified function/procedure, hiding all others."
  (interactive)
  (galec-goto-defun)
  (galec-hide-other-defuns))

 ;; hide/show annotations
(make-local-variable 'line-move-ignore-invisible)
(setq line-move-ignore-invisible t)
(if (functionp 'add-to-invisibility-spec)
    (add-to-invisibility-spec '(galec-annotation . t))
  ;; XEmacs 21.1 does not know function add-to-invisibility-spec
  (make-local-variable 'buffer-invisibility-spec)
  (setq buffer-invisibility-spec '((galec-annotation . t))))


(defun galec-hide-annotations (beg end)
  "Hide all annotations."
  (save-excursion
    (let (beg-hide end-hide)
      (goto-char beg)
      (while
	  (and (< (point) end)
	       (search-forward-regexp "\\<annotation[ \t\n]*\(" end t))
	(setq beg-hide (match-end 0))
	(backward-char)
	(forward-sexp)
	(setq end-hide (- (point) 1))
	(galec-flag-region beg-hide end-hide t)))))

(defun galec-show-annotations (beg end)
  "Show annotations from beg to end"
  (galec-flag-region beg end nil))

(defun galec-hide-all-annotations ()
  "Hide all annotations"
  (interactive)
  (galec-hide-annotations (point-min) (point-max)))

(defun galec-hide-annotation ()
  "Hide annotation of current statement"
  (interactive)
  (save-excursion
    (let (beg end)
      ;; move to beginning of current statement
      (galec-statement-start)
      (setq beg (point))
      ;; move to beginning of next statement
      (galec-forward-statement)
      (setq end (point))
      ;; hide annotations from beg to end
      (galec-hide-annotations beg end))))

(defun galec-show-all-annotations ()
  "Show all annotations"
  (interactive)
  (galec-show-annotations (point-min) (point-max)))

(defun galec-show-annotation ()
  "Show annotation of current statement"
  (interactive)
  (save-excursion
    (let (beg end)
      ;; move to beginning of current statement
      (galec-statement-start)
      (setq beg (point))
      ;; move to beginning of next statement
      (galec-forward-statement)
      (setq end (point))
      ;; show annotations from beg to end
      (galec-show-annotations beg end))))

(defun galec-discard-overlays (beg end value)
  (if (< end beg)
      (setq beg (prog1 end (setq end beg))))
  (save-excursion
    (let ((overlays (overlays-in beg end))
	  o)
      (while overlays
	(setq o (car overlays))
 	(if (eq (overlay-get o 'invisible) value)
	    (delete-overlay o))
	(setq overlays (cdr overlays))))))

(defun galec-flag-region (from to flag)
  "Hides or shows lines from FROM to TO, according to FLAG.
If FLAG is nil then text is shown, while if FLAG is t the text is hidden."
  (save-excursion
    (goto-char from)
    (galec-discard-overlays from to 'galec-annotation)
    (if flag
	(let ((o (make-overlay from to)))
	  (overlay-put o 'invisible 'galec-annotation)
	  (overlay-put o 'isearch-open-invisible
		       'galec-isearch-open-invisible)))))


  (defvar galec-mode-map nil
    "Keymap for Galec mode.")

  (if galec-mode-map ()
    (setq galec-mode-map (make-sparse-keymap))
    (define-key galec-mode-map "\C-j"	'galec-newline-and-indent)
    (define-key galec-mode-map "\C-c\C-e"	'galec-insert-end)
    (define-key galec-mode-map "\C-c\C-s"	'galec-show-annotation)
    (define-key galec-mode-map "\C-c\C-h"	'galec-hide-annotation)
    (define-key galec-mode-map "\es"	        'galec-show-all-annotations)
    (define-key galec-mode-map "\eh"	        'galec-hide-all-annotations)
    (define-key galec-mode-map "\C-c\C-c"	'comment-region)
    (define-key galec-mode-map "\e;"         'galec-indent-for-comment)
    (define-key galec-mode-map "\ej"         'galec-indent-new-comment-line)
    (define-key galec-mode-map "\ef"         'galec-forward-statement)
    (define-key galec-mode-map "\eb"         'galec-backward-statement)
    (define-key galec-mode-map "\en"         'galec-forward-block)
    (define-key galec-mode-map "\ep"         'galec-backward-block)
    (define-key galec-mode-map "\ea"         'galec-to-block-begin)
    (define-key galec-mode-map "\ee"         'galec-to-block-end))



  (defvar galec-mode-menu
    '("Galec"
      ("Move to"
       [" - next statement"        galec-forward-statement t]
       [" - previous statement"    galec-backward-statement t]
       [" - start of code block"   galec-to-block-begin t]
       [" - end of code block"     galec-to-block-end t]
       )
      [" - next code block"        galec-forward-block t]
      [" - previous code block"    galec-backward-block t]
      "-"
      ("Annotation"
       [" - show all"              galec-show-all-annotations t]
       [" - hide all"              galec-hide-all-annotations t]
       )
      [" - show current"          galec-show-annotation t]
      [" - hide current"          galec-hide-annotation
       :keys "C-c C-h" :active t]
      "-"
      ("Indent"
       [" - for comment"           galec-indent-for-comment t]
       ["Newline and indent"       galec-newline-and-indent
        :keys "C-j" :active t]
       ["New comment line"         galec-indent-new-comment-line t]
       )
      [" - line"                   indent-for-tab-command t]
      [" - region"                 indent-region (mark)]
      "-"
      ["Comment out region"        comment-region  (mark)]
      ["Uncomment region"          (comment-region (point) (mark) '(4))
       :keys "C-u C-c C-c" :active (mark)]
      "-"
      ["End code block"            galec-insert-end t]
      )
    "Menu for Galec mode.")

(provide 'galec-mode)

;;; galec-mode.el ends here
