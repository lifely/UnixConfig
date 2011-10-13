;; ----------------------------------------------------------------------
;; File: display.el - setup look and feel for my emacs
;;                         (scrolling, fonts, color theme etc.)
;;       Part of my emacs configuration (see ~/.emacs or init.el)
;;
;; Creation:  08 Jan 2010
;; Time-stamp: <Fri 2011-09-23 22:59 lifely>
;;
;; Copyright (c) 2010 Julien Di Marco <juliendimarco@me.com>
;;               http://julien.frenchlabs.net
;;
;; More information about Emacs Lisp:
;;              http://www.emacswiki.org/emacs/EmacsLisp
;; ----------------------------------------------------------------------


;; NxHtml Load

(load "~/.emacs.d/site-lisp/css-mode.el")
(load "~/.emacs.d/site-lisp/less-mode.el")
;(load "~/.emacs.d/site-lisp/nxml-mode-20041004/rng-auto.el")
;(load "~/.emacs.d/site-lisp/nxhtml/autostart.el")

;; === Automode alist ===
;; list of filename patterns vs. corresponding major mode functions
(setq auto-mode-alist
      (append
       '(("\\.pov$"				. pov-mode)
         ("\\.c$"				. c-mode)
         ("\\.pod$"				. pod-mode)
         ("\\.\\(hh\\|cpp\\|cc\\|hpp\\|cxx\\)$" . c++-mode)
         ("\\.\\(wml\\|htm\\|html\\|xhtml\\)$"  . html-mode)
         ("\\.\\(diffs?\\|patch\\|rej\\)\\'"    . diff-mode)
         ("\\.\\(pl\\|pm\\|cgi\\)$"             . cperl-mode)
         ("\\.gnuplot$"				. gnuplot-mode)
         ("\\.plot$"				. gnuplot-mode)
         ("\\.php$"				. php-mode)
         ("\\.css$"				. css-mode)
         ("\\.less$"				. less-css-mode)
         ("\\.cws$"				. tuareg-mode)
         ("\\.cwp$"				. tuareg-mode)
         ("\\.md$"				. markdown-mode)
         ("\\.rake$"				. ruby-mode)
         ("\\.gemspec$"				. ruby-mode)
         ("\\.rb$"				. ruby-mode)
         ("Rakefile$"				. ruby-mode)
         ("Gemfile$"				. ruby-mode)
         ("Capfile$"				. ruby-mode)
         (".ssh/config\\'"			. ssh-config-mode)
         ("sshd?_config\\'" .			ssh-config-mode)
         ("^TODO"				. change-log-mode))
       auto-mode-alist))

;; list of interpreters specified in the first line (starts with `#!')
(setq interpreter-mode-alist
      (append
       `(("perl"   . cperl-mode)
         ("expect" . tcl-mode)
         ;;("bash" . sh-mode)
         ) interpreter-mode-alist))


;; Tuareg Mode + ocaml

(setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'caml-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)


;; Espresso Mode
;; (autoload #'espresso-mode "espresso" "Start espresso-mode" t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
;; (add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))

(provide 'default)
;; ----------------------------------------------------------------------
;; eof