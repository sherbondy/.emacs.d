(set-face-attribute 'default nil :font "mono-10")

;; hide the toolbar
(tool-bar-mode -1)

;; show matching paren
(show-paren-mode 1)

;; marmalade
;; usage: M-x package-install
;; nrepl, solarized-theme, exec-path-from-shell
(require 'package)

(defvar marmalade '("marmalade" . "http://marmalade-repo.org/packages/"))
(defvar melpa     '("melpa" . "http://melpa.milkbox.net/packages/"))
(defvar gnu       '("gnu" . "http://elpa.gnu.org/packages"))

(add-to-list 'package-archives marmalade)
(add-to-list 'package-archives melpa t)
(add-to-list 'package-archives gnu)

(package-initialize)

(defun packages-install (&rest packages)
  (mapc (lambda (package)
          (let ((name (car package))
                (repo (cdr package)))
            (when (not (package-installed-p name))
              (let ((package-archives (list repo)))
                (package-install name)))))
        packages))

(defun init--install-packages ()
  (packages-install
   (cons 'exec-path-from-shell melpa)
   (cons 'paredit melpa)
   (cons 'gist melpa)
   (cons 'clojure-mode melpa)
   (cons 'clojure-test-mode melpa)
   (cons 'nrepl melpa)
   (cons 'solarized-theme melpa)))

(init--install-packages)

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

(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

(setq TeX-source-correlate-method 'synctex)

(setq inferior-lisp-program "browser-repl")

(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\.cljx$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; switch between header and implementation: C-c o
(add-hook 'c-mode-common-hook
  (lambda() 
    (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

(setq-default indent-tabs-mode nil)
(setq c-basic-indent 4)
(setq tab-width 4)

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'scheme-mode-hook           (lambda () (paredit-mode +1)))
(add-hook 'clojure-mode-hook          (lambda () (paredit-mode +1)))

(setq scmutils-root "/usr/local/scmutils")

(setq scheme-program-name
      (concat scmutils-root
      "/mit-scheme/bin/scheme "
      "-library " scmutils-root "/mit-scheme/lib "
      "-heap 6500"))
