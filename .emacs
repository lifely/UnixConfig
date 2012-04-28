;; -------------------------------------------------------------------------
;; .emacs -- my personnal Emacs Init File
;;            see http://julien.frenchlabs.net
;;
;; Copyright (c) 1991-2011 Julien Di Marco <juliendimarco@me.com>
;;               http://julien.frenchlabs.net
;;
;; -------------------------------------------------------------------------
;;
;;    ___ _ __ ___   __ _  ___ ___
;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;  |  __/ | | | | | (_| | (__\__ \
;; (_)___|_| |_| |_|\__,_|\___|___/
;;
;;
;;
;; -------------------------------------------------------------------------

;; =====================
;; OLD BOCAL STUFF
;; =====================

;; BOCAL STAFF 2010-2011
;; FEDORA DUMP

;;(load "std.el")
;;(load "std_comment.el")
;;(if (file-exists-p "~/.myemacs")
;;   (load-file "~/.myemacs"))

;; END BOCAL CONFIG

;; =========================
;; Environment determination
;; =========================

;; === Load path etc. ===

(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/color-theme/")              ; color-theme Themes
(add-to-list 'load-path "~/.emacs.d/site-lisp/")                ; external elisp files
(add-to-list 'load-path "~/.emacs.d/site-lisp/color-theme/")    ; Color-Theme Porta

;; =====================
;; Definition & Load
;; =====================
 (require 'school)                ; see  ~/.emacs.d/school.el
 (require 'school-advanced)       ; see  ~/.emacs.d/school-advanced.el

 (require 'default)               ; see  ~/.emacs.d/default.el
 (require 'display)               ; see  ~/.emacs.d/display.el
 (require 'defuns)                ; see  ~/.emacs.d/defuns.el
 (require 'bindings)              ; see  ~/.emacs.d/bindings.el

;; =====================
;; General Emacs Options
;; =====================

;; == UTF-8 Unicode Encoding
(prefer-coding-system 'utf-8)


;; === User authentication ===
(setq user-full-name    "Julien Di Marco")
(setq user-mail-address "<juliendimarco@me.com>")
;; for the webpage url, see Auto-insert section

;; Turn off mouse interface early in staptup to avoid momentary display
;; You really don't need these (except perhaps the menu-bar); trust me.
;;(if (fboundp 'menu-bar-mode) (menu-bar-mode nil))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode nil))

;; scroll bar may be useful - replace 't' by 'nil' to disable right scrollbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode nil))

(setq buffers-menu-max-size             nil)         ; no buffer max-size
(setq truncate-partial-width-windows    nil)

(setq line-number-mode          t)
(setq column-number-mode        t)
(setq search-highlight          t)      ; highlight search object
(setq query-replace-highlight   t)      ; highlight query object
(auto-compression-mode          t)      ; transparently edit compressed files
(column-number-mode             t)      ; column number in graphical
(setq byte-compile-verbose      nil)
;;(setq visible-bell            t)      ; Make screen blink at end & start - disturbing

(setq initial-major-mode 'text-mode)    ; to avoid autoloads for lisp mode
;;(setq require-final-newline   t)      ; ensure a file ends in a newline when it
                                        ; is saved

(setq show-trailing-whitespace          t)      ; color whitespace in red
(setq-default show-trailing-whitespace  t)

;; === Auto-save and backup files ===
(setq auto-save-list-file-name  nil)    ; no .saves files
(setq auto-save-default         t)      ; auto saving
(setq make-backup-files         nil)    ; make  backup files "~" (t = true - nil = false)

;; see http://www.emacswiki.org/emacs/BackupDirectory
(setq
 backup-by-copying t                    ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves"))                  ; don't litter my fs tree
 delete-old-versions t                  ; delete excess backup versions
                                        ; silently
 kept-new-versions 6
 kept-old-versions 2
 version-control t)                     ; make numeric backup versions
;; TODO: check out backup-dir

;; === Auto-fill configuration ===
;; automatic wrapping of lines and insertion of newlines when the cursor
;; goes over the column limit.
(setq-default fill-column       80)
(setq auto-fill-mode            t)      ; activate by default

;; ============================================================
;; These should be loaded on startup rather than autoloaded
;; on demand since they are likely to be used in every session

;; =====================
;; Additional Mode/Init
;; =====================

;; === line colums ===
(require 'linum)
(setq linum-format "%d ")
(global-linum-mode)
(setq linum-format
      (lambda (line)
        (propertize (format
                     (let ((w (length (number-to-string
                                       (count-lines (point-min) (point-max))))))
                       (concat "%" (number-to-string w) "d "))
                     line)
                    'face 'linum)))

;; === Better search form [Alex Strzel INSIDE] ===
(require 'iswitchb nil t)
(when (fboundp 'iswitchb-default-keybindings)
  (iswitchb-default-keybindings))

;; == Better search in file/buffer input
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;; === Htmlize Mode ===
(require 'htmlize)

;; === Sessions Saving Mode ===
(require 'saveplace)            ; Saving Emacs Sessions
(setq-default save-place t)     ; (cursor position etc. )

;; === Uniquify Mode for buffer Name ===
(require 'uniquify)     ; Unique buffer names dependent on file name
(setq uniquify-buffer-name-style 'forward) ; Use names with parts of directory

;; === Maintain last change time stamps (via Time-stamp: <>) ===
(require 'time-stamp)
;; format of the string inserted by `M-x time-stamp'
;; `Weekday YYYY-MM-DD HH:MM USER'
(setq time-stamp-format "%3a %:y-%02m-%02d %02H:%02M %u")
(add-hook 'write-file-hooks 'time-stamp) ; update at buffer-saving

;; === Show matching parenthesis ===
(require 'paren)
(show-paren-mode t)

(set-face-foreground 'show-paren-mismatch-face "red") ; set the scope color
(set-face-attribute  'show-paren-mismatch-face nil
                     :weight 'bold :underline t :overline nil :slant 'normal)

;; === Show whitespaces/tabs etc. ===

;; make whitespace-mode use “¶” for newline and “▷” for tab.
;; together with the rest of its defaults
(setq whitespace-display-mappings
 '(
   (space-mark 32 [183] [46]) ; normal space, ·
   (space-mark 160 [164] [95])
   (space-mark 2208 [2212] [95])
   (space-mark 2336 [2340] [95])
   (space-mark 3616 [3620] [95])
   (space-mark 3872 [3876] [95])
   (newline-mark 10 [182 10]) ; newlne, ¶
   (tab-mark 9 [8677 9] [92 9]) ; tab, ⇥
))

(setq x-stretch-cursor t)
(require 'show-wspace)
;;(add-hook 'font-lock-mode-hook 'show-ws-highlight-tabs)

;; === Indenting configuration ===
;; see http://www.emacswiki.org/emacs/IndentationBasics

(setq-default indent-tabs-mode  nil)           ; indentation can't insert tabs
(setq         indent-tabs-mode  nil)
(setq-default tab-width         2)
(setq         tab-width         2)
(setq indent-line-function      'insert-tab)
(setq tab-stop-list (number-sequence 4 120 4)) ; tab-to-tab stop list (M-i)
;;(setq c-brace-offset 2)

;; Overwrite major-mode - In fact mode like makefile are forced to do it
;; (add-hook 'after-change-major-mode-hook
;;           '(lambda ()
;;              (setq-default indent-tabs-mode  nil)           ; indentation can't insert tabs
;;              (setq         indent-tabs-mode  nil)
;;              (setq-default tab-width         2)
;;              (setq         tab-width         2)
;;              (setq indent-line-function      'insert-tab)
;;              (setq tab-stop-list (number-sequence 2 120 2))))

(defvaralias 'c-basic-offset      'tab-width)  ; use previous var
(defvaralias 'cperl-indent-level  'tab-width)

;; Automatic indentation of pasted text like in TextMate
;; See M-v command in init-bindings.el for yand-and-indent

;; === Auto-complete ===
;; Manual: http://cx4a.org/software/auto-complete/manual.html
;; EmacsWiki: http://www.emacswiki.org/emacs/AutoComplete
(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/site-lisp/ac-dict/")

;; = Loading Extension
(load-library "clang-completion-mode")
(require 'auto-complete-extension)

(if (not (eq system-type 'darwin))
    (ac-config-default)
  )

;; ==============================================================
;; Autopair: Automagically pair braces and quotes like TextMate
;; see http://code.google.com/p/autopair/ or
;; http://www.emacswiki.org/emacs/AutoPairs
;; ==============================================================

;;(require 'autopair)
;;(autopair-global-mode) ;; enable autopair in all buffers
;;(setq autopair-autowrap t)

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; EOF