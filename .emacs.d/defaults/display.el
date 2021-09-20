;; ----------------------------------------------------------------------
;; File: display.el - setup look and feel for my emacs
;;                         (scrolling, fonts, color theme etc.)
;;       Part of my emacs configuration (see ~/.emacs or init.el)
;;
;; Creation:  08 Jan 2010
;; Time-stamp: <Sun 2012-02-12 18:26 lifely>
;;
;; Copyright (c) 2010 Julien Di Marco <juliendimarco@me.com>
;;               http://julien.frenchlabs.net
;;
;; More information about Emacs Lisp:
;;              http://www.emacswiki.org/emacs/EmacsLisp
;; ----------------------------------------------------------------------

;; === Default size of the frame ===
;; (set-frame-width (selected-frame) 120)
;; (set-frame-height (selected-frame) 40)

;; === remove the few annoyance of default emacs ===
;; Use "y or n" answers instead of full words "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; remove initial message
(setq inhibit-startup-message t)

;; kill and move region directly
(delete-selection-mode t)
;;(pc-selection-mode)

;; === display current time in the status bar ===
(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)

;; === Specify the frame title ===
;; see http://www.emacswiki.org/emacs/FrameTitle
;; recognize the same special characters as mode-line-format variable, mainly:
;;    %b -- print buffer name.      %f -- print visited file name.
;;    %F -- print frame name.
;;    %* -- print %, * or hyphen.   %+ -- print *, % or hyphen.
;;          %& is like %*, but ignore read-only-ness.
;;          % means buffer is read-only and * means it is modified.
;;          For a modified read-only buffer, %* gives % and %+ gives *.
;;    %m -- print the mode name.
;;    %z -- print mnemonics of buffer, terminal, and keyboard coding systems.
;;    %Z -- like %z, but including the end-of-line format.
;;    %[ -- print one [ for each recursive editing level.  %] similar.
;;    %% -- print %.   %- -- print infinitely many dashes.
;;  Decimal digits after the % specify field width to which to pad.
(setq frame-title-format '(buffer-file-name "emacs: %b (%f)" "emacs: %b"))

;; =================================================================
;; Font selection (to use a mono-spaced (non-proportional) font)
;; =================================================================
;; Snow Leopard users may try Menlo-12, other should consider Monaco-12.
(add-to-list 'default-frame-alist '(font . "Anonymous-Pro-8"))

;; =================================================================
;; Emacs Color Theme
;; see http://www.emacswiki.org/emacs/ColorTheme
;; see http://code.google.com/p/gnuemacscolorthemetest/ for direct
;; screenshots -
;; =================================================================
;; ============= Must Have [xterm-256color] in TERM env ============
;; THEME loading

;;(require 'color-theme-blackboard)
(require 'color-theme-tangotango)
(require 'color-theme-gruber-darker)
(require 'color-theme-less)
(require 'color-theme-mac-classic)
(require 'color-theme-subdued)
(require 'color-theme-tango)
(require 'color-theme-almost-monokai)

;; Basic Conf

(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-tangotango)))

;; linum background color

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:inherit (shadow default) :background "#222222")))))

;; WITH color theme
;;(require 'color-theme)

;; (color-theme-initialize)
;; (setq color-theme-is-global t)
;;  (color-theme-aalto-light)
;;  (color-theme-vim-colors)
;;  (color-theme-tango)
;;  (color-theme-tangotango)

;; WITHOUT color theme
;; (set-background-color "lightyellow")
;; (setq default-frame-alist
;;      '((cursor-color . "green")
;;        (cursor-type . box)))
;; (set-default 'cursor-type 'box)


;; Highlight Current line
;; see [http://emacsblog.org/2007/04/09/highlight-the-current-line/]
(global-hl-line-mode 1)

;;To customize the background color
(set-face-background 'hl-line "#222")  ;; Emacs 22 Only
;;(set-face-background 'highlight "#330")  ;; Emacs 21 Only

;; Selection Highlight on mac os x
(cua-mode               t)
(cua-selection-mode     t)

(provide 'display)
;; ----------------------------------------------------------------------
;; eof