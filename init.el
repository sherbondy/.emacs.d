(set-face-attribute 'default nil :font "Monaco-14")

;; hide the toolbar
(tool-bar-mode -1)

;; marmalade
;; usage: M-x package-install
;; nrepl, solarized-theme
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; oh yeah
(load-theme 'solarized-dark t)
