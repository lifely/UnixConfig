;; ----------------------------------------------------------------------
;; File: bindings.el - setup my key bindings in emacs
;;       Part of my emacs configuration (see ~/.emacs or init.el)
;;
;; Creation:  01 Mar 2011
;; Time-stamp: <Fri 2011-04-22 21:32 di-mar_j>
;;
;; Copyright (c) 2010 Julien Di Marco <juliendimarco@me.com>
;;               http://julien.frenchlabs.net
;;
;; More information about Emacs Lisp:
;;              http://www.emacswiki.org/emacs/EmacsLisp
;; ----------------------------------------------------------------------

;; === Buffer switching ===
;; C-x b permits to switch among the buffer by entering a buffer name,
;; with completion.
;; See http://www.emacswiki.org/emacs/IswitchBuffers

(require 'iswitchb)
(iswitchb-mode t)

;; to ignore the *...* special buffers from the list
(setq iswitchb-buffer-ignore '("^ " "*Buffer"))

;; Move from one buffer to another using 'C-<' and 'C->'

;;(load "cyclebuffer" nil 't)
;;(global-set-key (kbd "C-<") 'cyclebuffer-forward)
;;(global-set-key (kbd "C->") 'cyclebuffer-backward)
(global-set-key (kbd "C-<") 'previous-buffer)
(global-set-key (kbd "C->") 'next-buffer)

;; Left - Right Arrow for buffer Switch
(defun iswitchb-local-keys ()
  (mapc (lambda (K)
          (let* ((key (car K)) (fun (cdr K)))
            (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
        '(("<right>" . iswitchb-next-match)
          ("<left>"  . iswitchb-prev-match)
          ("<up>"    . ignore             )
          ("<down>"  . ignore             ))))
(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

;; === Window switching ===
(global-set-key [C-prior] 'other-window)
(global-set-key [C-next]  'other-window)

;; === Multi speed mouse scrolling ===
;; scroll:         normal speed
;; Ctrl + scroll:  high speed
;; Shift + scroll: low  speed

(defun up-slightly () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))
(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)

(defun up-one () (interactive) (scroll-up 1))
(defun down-one () (interactive) (scroll-down 1))
(global-set-key [S-mouse-4] 'down-one)
(global-set-key [S-mouse-5] 'up-one)

(defun up-a-lot () (interactive) (scroll-up))
(defun down-a-lot () (interactive) (scroll-down))
(global-set-key [C-mouse-4] 'down-a-lot)
(global-set-key [C-mouse-5] 'up-a-lot)

;; === Navigation ===
(global-set-key [kp-home]  'beginning-of-buffer) ; [Home]
(global-set-key [home]     'beginning-of-buffer) ; [Home]
(global-set-key [kp-end]   'end-of-buffer)       ; [End]
(global-set-key [end]      'end-of-buffer)       ; [End]

;; goto next error (raised in the compilation buffer typically)
(global-set-key (kbd "C-x n") 'next-error)
(global-set-key (kbd "C-x p") 'previous-error)

(global-set-key (kbd "M-n") 'goto-line)          ; goto line number
(global-set-key (kbd "M-l") 'goto-line)          ; goto line number
(global-set-key (kbd "M-g") 'goto-line)          ; goto line number

;; miscelenaous

(global-set-key (kbd "C-c C-p") 'match-paren)

;; Jump to a definition in the current file. (This is awesome.)
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; === Compilation ===
(global-set-key (kbd "C-x C-e") 'smart-compile)

;; === Kill this buffer ===
(global-set-key (kbd "C-q") 'kill-this-buffer)

;; === Launch a shell ===
(global-set-key (kbd "C-!") 'shell)

;; === Re-indent the full file (quite useful) ===
(global-set-key (kbd "C-x i") 'indent-buffer)  ; see ~/.emacs.d/init-defuns
;(global-set-key (kbd "C-x o") 'indent-buffer)  ; see ~/.emacs.d/init-defuns

;; === yank and indent copied region ===
(global-set-key (kbd "M-v")  'yank-and-indent)

;; === Block Folding [hs hide/show] ===
(add-hook 'c-mode-common-hook 'hs-minor-mode t)
;; ^ Activation

(global-set-key [f11] 'hs-toggle-hiding)	;; Hide block
(global-set-key [f12] 'hs-show-block)		;; Show block

(global-set-key [(shift f11)] 'hs-hide-all)	;; Hide all
(global-set-key [(shift f12)] 'hs-show-all)	;; Show all

;; === Search [and replace] ===
;; Use regex searches by default.
(global-set-key (kbd "C-M-s")   'isearch-forward)
(global-set-key (kbd "C-M-r")	'isearch-backward)
(global-set-key (kbd "C-s")	'isearch-forward-regexp)
(global-set-key (kbd "\C-r")	'isearch-backward-regexp)
(global-set-key (kbd "M-q")	'query-replace)
(global-set-key (kbd "M-5")	' query-replace-regexp)

;; === Emacs Org ===
;; An Emacs Mode for Notes, Project Planning, and Authoring
;; see http://www.emacswiki.org/emacs/OrgMode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; === Flyspell ===
(global-set-key (kbd "C-c C-i w")  'ispell-word)
(global-set-key (kbd "C-c C-i b")  'ispell-buffer)

;; Easy comment or uncomment
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)

;; === Buffer Manipulation ===
;; Resizing
(global-set-key [(control f5)] 'shrink-window-horizontally)
(global-set-key [(control f8)] 'enlarge-window-horizontally)
(global-set-key [(control f6)] 'shrink-window)
(global-set-key [(control f7)] 'enlarge-window)

;; Moving
(global-set-key [(control meta left)] 'windmove-left)          ; move to left windnow
(global-set-key [(control meta right)] 'windmove-right)        ; move to right window
(global-set-key [(control meta up)] 'windmove-up)              ; move to upper window
(global-set-key [(control meta down)] 'windmove-down)          ; move to downer window

;; === Old Key Bindings ===

(global-set-key [delete] 'delete-backward-char)
(global-set-key [kp-enter] 'newline-and-indent)

(global-set-key [f6] 'toggle-line-numbers-display)
(global-set-key [(shift f6)] 'global-linum-mode)

(global-set-key [f5] 'svn-status)
(global-set-key [f7] 'kill-this-buffer)
(global-set-key [f8] 'speedbar)
(global-set-key [f9] 'compile)
;;(global-set-key [(control z)] 'undo)

;; Delete current buffer and do not ask which one with C-x k
(global-set-key [(control x) (k)] 'kill-this-buffer)

(provide 'bindings)
;; ----------------------------------------------------------------------
;; eof