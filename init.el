;; must load cedet early on before eieio gets loaded
(load-file "~/emacs/cedet-bzr/cedet-devel-load.el")

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
(defvar org       '("org" . "http://orgmode.org/elpa/"))

(add-to-list 'package-archives marmalade t)
(add-to-list 'package-archives melpa t)
(add-to-list 'package-archives gnu t)
(add-to-list 'package-archives org t)

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
   (cons 'auto-complete melpa)
   (cons 'paredit melpa)
   (cons 'gist melpa)
   (cons 'clojure-mode melpa)
   (cons 'clojure-test-mode melpa)
   (cons 'nrepl melpa)
   (cons 'ac-nrepl melpa)
   (cons 'solarized-theme melpa)
   (cons 'rainbow-delimiters melpa)
   (cons 'markdown-mode melpa)
   (cons 'haml-mode melpa)
   (cons 'scss-mode melpa)
   (cons 'scala-mode2 melpa)
   (cons 'auto-complete-clang melpa)
   (cons 'magit melpa)
   (cons 'cljsbuild-mode melpa)
   (cons 'org org)
   (cons 'undo-tree melpa)))

(init--install-packages)

(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

;; oh yeah
(load-theme 'solarized-dark t)

;; show line numbers!
(global-linum-mode t)

;; Enable eldoc in clojure buffers:
(add-hook 'nrepl-interaction-mode-hook
  'nrepl-turn-on-eldoc-mode)

;; Stop the error buffer from popping up while working in the REPL buffer:
(setq nrepl-popup-stacktraces nil)

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

(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\.cljx$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.haml\\'" . haml-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;; switch between header and implementation: C-c o
(add-hook 'c-mode-common-hook
  (lambda() 
    (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

(setq-default indent-tabs-mode nil)
(setq c-basic-indent 4)
(setq tab-width 4)

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       'paredit-mode)
(add-hook 'lisp-mode-hook             'paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook           'paredit-mode)
(add-hook 'clojure-mode-hook          'paredit-mode)
(add-hook 'nrepl-mode-hook            'paredit-mode)


;; kibit setup
;; Teach compile the syntax of the kibit output
(require 'compile)
(add-to-list 'compilation-error-regexp-alist-alist
         '(kibit "At \\([^:]+\\):\\([[:digit:]]+\\):" 1 2 nil 0))
(add-to-list 'compilation-error-regexp-alist 'kibit)

;; A convenient command to run "lein kibit" in the project to which
;; the current emacs buffer belongs to.
(defun kibit ()
  "Run kibit on the current project.
   Display the results in a hyperlinked *compilation* buffer."
  (interactive)
  (compile "lein kibit"))


;; markdown
(autoload 'markdown-mode "markdown-mode.el" 
  "Major mode for editing Markdown files" t) 

(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))


;; scheme setup
(require 'quack)
(setq quack-default-program "csi")

(setq scmutils-root "/usr/local/scmutils")
(setq scheme-program-name
      (concat scmutils-root
      "/mit-scheme/bin/scheme "
      "-library " scmutils-root "/mit-scheme/lib "
      "-heap 6500"))

;; jsim setup

(autoload 'jsim-mode "jsim.el" nil t)
(setq auto-mode-alist (cons '("\.jsim$" . jsim-mode) auto-mode-alist))
(add-hook 'jsim-mode-hook 'turn-on-font-lock)


;; autocomplete setup

(require 'auto-complete-config)
(ac-config-default)

;; cedet config

(semantic-mode 1)
(require 'semantic/ia)
(require 'semantic/bovine/gcc)
(require 'semantic/db-javap)

;; to tell semantic where additional libs live, do:
;; (semantic-add-system-include "/the/include/dir" c++-mode)

;; imenu support for semantic tags
(defun imenu-semantic-hook ()
  (imenu-add-to-menubar "TAGS"))

(add-hook 'semantic-init-hooks 'imenu-semantic-hook)

;; ede projects mode from cedet
(global-ede-mode t)

;; use (C-c, TAB) for Senator name completion

(defun my-c-mode-cedet-hook ()
 (local-set-key "." 'semantic-complete-self-insert)
 (local-set-key ">" 'semantic-complete-self-insert))

(add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)
;; Evaluation of this code will lead to execution of the semantic-complete-self-insert command when user will press . or > after variables, that are instances of some data structure, and displaying a list of possible completions for given class or structure.
