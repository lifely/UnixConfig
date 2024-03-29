;; ----------------------------------------------------------------------
;; File: defuns.el - Definition of custom elisp functions
;;       Part of my emacs configuration (see ~/.emacs or init.el)
;;
;; Copyright (c) 2000-2010 Julien Di Marco <juliendimarco@me.com>
;;               http://julien.frenchlabs.net
;;
;; -------------------------------------------------------------------------
;;      _       __                       _
;;   __| | ___ / _|_   _ _ __  ___   ___| |
;;  / _` |/ _ \ |_| | | | '_ \/ __| / _ \ |
;; | (_| |  __/  _| |_| | | | \__ \|  __/ |
;;  \__,_|\___|_|  \__,_|_| |_|___(_)___|_|
;;
;; ----------------------------------------------------------------------

(require 'thingatpt)
(require 'imenu)

;; === Word count ===
;; (defun word-count nil
;;  "Count words in buffer"
;;  (interactive)
;;  (shell-command-on-region (point-min) (point-max) "wc -w"))

;; Courtesy of Evan Sultanik (http://www.sultanik.com/Word_count_in_Emacs)
;; quote: "I wrote a relatively simple (and equally lazy) Emacs Lisp function to
;;         calculate word length. It even strips LaTeX files of their commands!"
(defun word-count (&optional filename)
  "Returns the word count of the current buffer.  If `filename' is not nil, returns the word count of that file."
  (interactive)
  (save-some-buffers) ;; Make sure the current buffer is saved
  (let ((tempfile nil))
    (if (null filename)
        (progn
          (let ((buffer-file (buffer-file-name))
                (lcase-file (downcase (buffer-file-name))))
            (if (and (>= (length lcase-file) 4) (string= (substring lcase-file -4 nil) ".tex"))
                ;; This is a LaTeX document, so DeTeX it!
                (progn
                  (setq filename (make-temp-file "wordcount"))
                  (shell-command-to-string (concat "detex < " buffer-file " > " filename))
                  (setq tempfile t))
              (setq filename buffer-file)))))
    (let ((result (car (split-string (shell-command-to-string (concat "wc -w " filename)) " "))))
      (if tempfile
          (delete-file filename))
      (message (concat "Word Count: " result))
      )))

;; === find a word definition ===
(defun word-definition-lookup ()
  "Look up the word under cursor in a browser."
  (interactive)
  (browse-url
   (concat "http://www.answers.com/main/ntquery?s="
           (thing-at-point 'word))))


;; === Indentation of the full buffer ===
;; Courtesy from http://emacsblog.org/2007/01/17/indent-whole-buffer/
(defun indent-buffer ()
  "indent whole buffer"
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

;; === Yank (copy) and indent the copied region
;; see http://www.emacswiki.org/emacs/AutoIndentation
(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))

;; === unindent ===
(defun unindent-region ()
  (interactive)
  (save-excursion
	(if (< (point) (mark)) (exchange-point-and-mark))
	(let ((save-mark (mark)))
	  (if (= (point) (line-beginning-position)) (previous-line 1))
	  (goto-char (line-beginning-position))
	  (while (>= (point) save-mark)
		(goto-char (line-beginning-position))
		(if (= (string-to-char "\t") (char-after (point))) (delete-char 1))
		(previous-line 1)))))

;; === Load path related ===
(defun load-local-site-start (site-lisp-directory)
  "Load site-start.el from a given site-lisp directory"
  (let ((current-default-directory default-directory))
    (setq default-directory site-lisp-directory)
    (normal-top-level-add-subdirs-to-load-path)
    (setq default-directory current-default-directory)
    (setq load-path (cons site-lisp-directory load-path))
    (load (concat site-lisp-directory "/site-start.el"))
    ))

;; === Network ===
(defun view-url ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    ;; TODO: switch to nxml/nxhtml mode
    (cond ((search-forward "<?xml" nil t) (xml-mode))
          ((search-forward "<html" nil t) (html-mode)))))

;; === Buffer-related ===

(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
Symbols matching the text at point are put first in the completion list."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols (delq nil (mapcar (lambda (symbol)
                                                     (if (string-match regexp symbol) symbol))
                                                   symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
                  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun sudo-edit (&optional arg)
  "Edit a file as root using sudo"
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun toggle-fullscreen ()
  (interactive)
  ;; TODO: this only works for X. patches welcome for other OSes.
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

(defun switch-or-start (function buffer)
  "If the buffer is current, bury it, otherwise invoke the function."
  (if (equal (buffer-name (current-buffer)) buffer)
      (bury-buffer)
    (if (get-buffer buffer)
        (switch-to-buffer buffer)
      (funcall function))))

;; === attempt to load a feature/library, failing silently ===
(defun try-require (feature)
  "Attempt to load a library or module. Return true if the
library given as argument is successfully loaded. If not, instead
of an error, just add the package to a list of missing packages."
  (condition-case err
      ;; protected form
      (progn
        (message "Checking for library `%s'..." feature)
        (if (stringp feature)
            (load-library feature)
          (require feature))
        (message "Checking for library `%s'... Found" feature))
    ;; error handler
    (file-error  ; condition
     (progn
       (message "Checking for library `%s'... Missing" feature)
       (add-to-list 'missing-packages-list feature))
     nil)))

;; find matching parenthesis (% command in vim)
(defun match-paren (arg)
  "Go to the matching parenthesis, if on parenthesis; otherwise,
insert `%'."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; complement or indent on TAB=C-i
(defun th-complete-or-indent (arg)
  "If preceding character is a word character and the following character is a whitespace or non-word character, then
  `dabbrev-expand', else indent according to mode."
  (interactive "*P")
  (cond ((and
          (= (char-syntax (preceding-char)) ?w)
          (looking-at (rx (or word-end (any ".,;:#=?()[]{}")))))
         (require 'sort)
         (let ((case-fold-search t))
           (dabbrev-expand arg)))
        (t
         (indent-according-to-mode))))

;; to activate or not ECB
(defun ecb-toggle ()
  "Activate (or desactivate) Emacs Code Browser (ECB)"
  (interactive)
  (if ecb-minor-mode
      (ecb-deactivate)
    (ecb-activate)))

;; Function to launch the 'open' command on a file selected in dired
;; If on Ubuntu, you should symlink /usr/bin/open to /usr/bin/gnome-open
;;    cd /usr/bin && sudo ln -s gnome-open open && cd -
(defun dired-open-with-open ()
  (interactive)
  (let ((file-name (dired-get-file-for-visit)))
    (if (file-exists-p file-name)
        (call-process "/usr/bin/open" nil 0 nil file-name))))

;; remove the compilation buffer if there was no error
;; see http://www.emacswiki.org/emacs/ModeCompile
(setq compilation-exit-message-function
        (lambda (status code msg)
          ;; If M-x compile exists with a 0
          (when (and (eq status 'exit) (zerop code))
            ;; then bury the *compilation* buffer, so that C-x b doesn't go there
  	  (bury-buffer "*compilation*")
  	  ;; and return to whatever were looking at before
  	  (replace-buffer-in-windows "*compilation*"))
          ;; Always return the anticipated result of compilation-exit-message-function
  	(cons msg code)))

;; Winner mode version of this function - excellent but not compatible with ECB
;; (setq compilation-finish-functions 'compile-autoclose)
;;   (defun compile-autoclose (buffer string)
;;      (cond ((string-match "finished" string)
;; 	  (bury-buffer "*compilation*")
;;           (winner-undo)
;;           (message "Build successful."))
;;          (t
;;           (message "Compilation exited abnormally: %s" string))))
;; Alternative version
;; (setq compilation-finish-functions 'compile-autoclose)
;; (defun compile-autoclose (buffer string)
;;   (cond ((string-match "finished" string)
;; 		 (message "Build maybe successful: closing window.")
;; 		 (run-with-timer 1 nil
;; 						 'delete-window
;; 						 (get-buffer-window buffer t)))
;; 		(t
;; 		 (message "Compilation exited abnormally: %s" string))))


;; useful for ruby-mode
;; see http://groups.google.com/group/emacs-on-rails/browse_thread/thread/ae87fc797822bf3
(defun ruby-insert-end ()
  "Insert \"end\" at point and reindent current line."
  (interactive)
  (insert "end")
  (ruby-indent-line t)
  (end-of-line))

(provide 'defuns)
;; ----------------------------------------------------------------------
;; eof
