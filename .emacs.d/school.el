;; ----------------------------------------------------------------------
;; File: school.el - setup look and feel for my emacs
;;                         (scrolling, fonts, color theme etc.)
;;       Part of my emacs configuration (see ~/.emacs or init.el)
;;
;; Creation:  08 Jan 2010
;; Time-stamp: <Mon 2011-06-13 10:58 di-mar_j>
;;
;; Copyright (c) 2010 Julien Di Marco <juliendimarco@me.com>
;;               http://julien.frenchlabs.net
;;
;; More information about Emacs Lisp:
;;              http://www.emacswiki.org/emacs/EmacsLisp
;; ----------------------------------------------------------------------

;; Orginial School.el file !

(defun do_insert_time ()
  (interactive)
  (insert-string (current-time-string)))
(set-variable 'c-argdecl-indent   0)

;; BackWard-delete is now binded to C^H
;; (normal-erase-is-backspace-mode nil)

(global-set-key "" 'backward-delete-char)
(global-set-key "" 'compile)
(global-set-key "" 'goto-line)

(provide 'school)
;; ----------------------------------------------------------------------
;; eof