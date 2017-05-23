(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

;;; Package Bootstrapping

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;;; Base Emacs Settings

(add-to-list 'load-path (concat user-emacs-directory "packages/"))
(add-to-list 'exec-path "/usr/local/bin")

(set-default 'truncate-lines t)

;; Hide the startup splash screen
(setq inhibit-startup-message t)

;; Removes scratch message
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)

;; Turn off annoying system bell
(setq ring-bell-function 'ignore)

;; Write backups to the backup directory in emacs.d
(setq backup-directory-alist
      `(("." . ,(expand-file-name
		 (concat temporary-file-directory "backups")))))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name
                (concat temporary-file-directory "temp")))))

;; Fix empty pasteboard error
(setq save-interprogram-paste-before-kill nil)

;; Full path in frame title
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

;; Auto-refresh buffers when edits occur outside emacs
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Transparently open compress files
(auto-compression-mode t)

;; Replace 'yes-or-no' with 'y-or-n'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Use UTF-8 by default
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(show-paren-mode 1)

;; Remove text in selection when inserting text
(delete-selection-mode 1)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)
(setq linum-format " %3d ")

(setq transient-mark-mode t)

;; Lines should be 80 characters wide, not 72 ???
(setq fill-column 80)

;; Smooth scroll (one line at a time)
(setq mouse-wheel-scroll-amount '(1 ((shift) 0.1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 15)

;; Nicer scrolling with mouse wheel/trackpad.
;; From Graphene
(unless (and (boundp 'mac-mouse-wheel-smooth-scroll) mac-mouse-wheel-smooth-scroll)
  (global-set-key [wheel-down] (lambda () (interactive) (scroll-up-command 1)))
  (global-set-key [wheel-up] (lambda () (interactive) (scroll-down-command 1)))
  (global-set-key [double-wheel-down] (lambda () (interactive) (scroll-up-command 2)))
  (global-set-key [double-wheel-up] (lambda () (interactive) (scroll-down-command 2)))
  (global-set-key [triple-wheel-down] (lambda () (interactive) (scroll-up-command 5)))
  (global-set-key [triple-wheel-up] (lambda () (interactive) (scroll-down-command 5))))

;; Scroll one line when hitting bottom of window
(setq scroll-conservatively 10000)

;; Change cursor
(setq-default cursor-type 'box)
(blink-cursor-mode -1)

;; Do not insert tabs
(setq-default indent-tabs-mode nil)

;; Navigate camelcase words
(global-subword-mode 1)

;; Turn off word wrap
(setq-default truncate-lines t)

;; Remove double space at end of sentence
(set-default 'sentence-end-double-space nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-auto-revert-mode t)
(global-linum-mode t)
(global-visual-line-mode t)

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;;; Small Packages

(use-package saveplace
  :ensure t
  :config
  (setq-default save-place t)
  (setq save-place-file (expand-file-name "places" user-emacs-directory)))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :bind
  (("s-u" . undo-tree-visualize)
   ("s-z" . undo)
   ("s-Z" . undo-tree-redo))
  :config
  (global-undo-tree-mode t)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))

(use-package company
  :ensure t
  :config
  (global-company-mode t)
  (setq company-global-modes '(not org-mode))
  (setq company-global-modes '(not csv-mode))

                                        ; Disable Lowercase
  (add-to-list 'company-dabbrev-code-modes 'web-mode)
  (add-to-list 'company-dabbrev-code-modes 'clojure-mode)

  (setq company-idle-delay 0.125)
  (setq company-minimum-prefix-length 1))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode t)
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))

  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (setq show-paren-style 'expression))

;; (use-package parinfer
;;   :ensure t
;;   :init
;;   (progn
;;     (setq parinfer-extensions
;;           '(defaults       ; should be included.
;;              pretty-parens  ; different paren styles for different modes.
;;              smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
;;              smart-yank))   ; Yank behavior depend on mode.
;;     (add-hook 'clojure-mode-hook #'parinfer-mode)
;;     (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
;;     (add-hook 'common-lisp-mode-hook #'parinfer-mode)
;;     (add-hook 'scheme-mode-hook #'parinfer-mode)
;;     (add-hook 'lisp-mode-hook #'parinfer-mode)))

(use-package diminish
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :bind
   (("C-c m t" . mc/mark-all-like-this)
    ("C-c m m" . mc/mark-all-like-this-dwim)
    ("C-c m l" . mc/edit-lines)
    ("C-c m e" . mc/edit-ends-of-lines)
    ("C-c m a" . mc/edit-beginnings-of-lines)
    ("C-c m n" . mc/mark-next-like-this)
    ("C-c m p" . mc/mark-previous-like-this)
    ("C-c m s" . mc/mark-sgml-tag-pair)
    ("C-c m d" . mc/mark-all-like-this-in-defun)))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  ;; Ignore dirs
  (setq projectile-globally-ignored-directories
        (append projectile-globally-ignored-directories '(".git" "out" "node_modules" "bower_components"))))

(use-package helm-projectile
  :ensure t)

(use-package helm-swoop
  :ensure t
  :bind
  (("C-s" . helm-swoop-without-pre-input)
   ("s-i" . helm-swoop-back-to-last-point))
  :config
  ;; Disable pre-input
  (setq helm-swoop-pre-input-function
        (lambda () "")))

;;; Helm

(use-package helm
  :ensure t
  :diminish helm-mode
  :config
  (require 'helm-config)
  (helm-mode t)

  (helm-autoresize-mode t)
  (setq helm-autoresize-max-height 60)
  ;; __Fuzzers__

  (setq helm-M-x-fuzzy-match        t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match    t
        helm-semantic-fuzzy-match   t
        helm-imenu-fuzzy-match      t
        helm-find-file-ignore-thing-at-point t) ;Ignore the path under cursor

  ;; From https://gist.github.com/antifuchs/9238468
  (setq helm-idle-delay 0.0             ; update fast sources immediately (doesn't).
        helm-input-idle-delay 0.01      ; this actually updates things
                                        ; reeeelatively quickly.
        helm-quick-update t
        helm-M-x-requires-pattern nil
        helm-ff-skip-boring-files t)

  ;; __Keybindings__

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "<enter>") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions using C-z
  (define-key helm-map (kbd "s-s") 'helm-grep-default-recurse-command)

  (setq helm-split-window-in-side-p t)

  (global-unset-key (kbd "C-x c")) ; Remove the default helm key

  (global-set-key (kbd "s-h") 'helm-command-prefix)

  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-<return>") 'helm-M-x)

  (global-set-key (kbd "M-v") 'helm-show-kill-ring)
  (global-set-key (kbd "s-y") 'helm-show-kill-ring)

  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "s-b")   'helm-mini)

  (global-set-key (kbd "C-x C-f") 'helm-find-files)

  (global-set-key (kbd "s-p") 'helm-projectile))

;;; Package modes

(use-package cider
  :ensure t
  :config
  (cider-mode t)
  (add-hook 'cider-mode-hook #'eldoc-mode)

  (setq cider-repl-use-pretty-printing t)
  (setq cider-repl-use-clojure-font-lock t)
  (setq cider-show-error-buffer nil)
  (setq cider-prompt-save-file-on-load nil)

  (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode))

(use-package clojure-mode
  :ensure t
  :config
  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)
    (fact 1)
    (safe->> 1)
    (safe-> 1)))

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.[s]?css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.avsc\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))

  (set-face-background 'web-mode-current-element-highlight-face "#53b3eb")
  (set-face-foreground 'web-mode-current-element-highlight-face "#ffffff")

  (set-face-background 'web-mode-current-column-highlight-face "#fafafa")
  (set-face-foreground 'web-mode-current-column-highlight-face nil)

  (setq web-mode-content-types-alist
        '(("json"    . "\\.json\\'")
          ("json"    . "\\.eslintrc\\'")
          ("json"    . "\\.avsc\\'")
          ("jsx"     . "\\.js[x]?\\'")))

  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-indent-style 2)

  (setq indent-tabs-mode nil)
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-indentation t)
  (setq web-mode-enable-auto-opening t)
  (setq web-mode-enable-auto-quoting t)

  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t))

(use-package prettier-js
  :load-path
  "packages/"
  :config
  (setq prettier-target-mode "web-mode")
  )

(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))

(use-package markdown-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(use-package csv-mode
  :ensure t
  :config
  )

(use-package indent-guide
  :ensure t
  :config
  (indent-guide-global-mode)
  (set-face-foreground 'indent-guide-face "#dadada")
  (set-face-background 'indent-guide-face nil)
  (setq indent-guide-char "|")
  (setq indent-guide-delay 0.75))

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode t)
  (git-gutter:linum-setup))

;;; Aliases
(defalias 'ff        'helm-findfiles)
(defalias 'files     'helm-findes)
(defalias 'imenu     'helm-semantic-or-imenu)
(defalias 'paste     'helm-show-kill-ring)
(defalias 'resume    'helm-resume)
(defalias 'search    'helm-occur)
(defalias 'clipboard 'helm-show-kill-ring)
(defalias 'comment   'comment-or-uncomment-region)
(defalias 'start     'beginning-of-line)
(defalias 'begin     'back-to-indentation)
(defalias 'errors    'flycheck-list-errors)
(defalias 'repl      'cider-jack-in)
(defalias 'repl      'cider)
(defalias 'kill      'kill-buffer)

;;; Shortcuts

(windmove-default-keybindings 'meta)
(global-set-key (kbd "S-<tab>")   'delete-indentation)
(global-set-key (kbd "s-/")       'comment-or-uncomment-region)
(global-set-key (kbd "s-w")       'kill-buffer)
(global-set-key (kbd "s-q")       'save-buffers-kill-terminal)
(global-set-key (kbd "C-g")       'goto-line)

;;; Navigation

(global-set-key (kbd "s-<up>")      'backward-paragraph)
(global-set-key (kbd "s-<down>")    'forward-paragraph)
(global-set-key (kbd "s-<right>")   'end-of-line)
(global-set-key (kbd "s-<left>")   'beginning-of-line)
(global-set-key (kbd "M-<up>") 'sp-backward-sexp)
(global-set-key (kbd "M-<down>")  'sp-forward-sexp)
(global-set-key (kbd "M-<right>")    'right-word)
(global-set-key (kbd "M-<left>")  'left-word)

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)

;;;; Fonts
(add-to-list 'default-frame-alist '(font . "Source Code Pro-15"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (prettier-js rjsx-mode imenu+ imenu-anywhere multiple-cursors helm-swoop helm-projectile projectile elisp--witness--lisp markdown-mode web-mode cider helm smartparens rainbow-delimiters flycheck company undo-tree use-package))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((((class color) (background light)) (:background "gray94")))))
