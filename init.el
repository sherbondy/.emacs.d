(set-face-attribute 'default nil :font "Monaco-14")

;; hide the toolbar
(tool-bar-mode -1)

;; marmalade
;; usage: M-x package-install
;; nrepl, solarized-theme, exec-path-from-shell
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; oh yeah
(load-theme 'solarized-dark t)

;; Enable eldoc in clojure buffers:
(add-hook 'nrepl-interaction-mode-hook
  'nrepl-turn-on-eldoc-mode)

;; Stop the error buffer from popping up while working in the REPL buffer:
(setq nrepl-popup-stacktraces nil)

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Setup environment variables from the user's shell.
(when is-mac (exec-path-from-shell-initialize))

;; make sure path is correct when launched as application
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(push "/usr/local/bin" exec-path)

;; load tex packages
(push "/usr/share/emacs/site-lisp" load-path)
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)
(setq-default TeX-master nil)

(setq TeX-output-view-style "open %o")

