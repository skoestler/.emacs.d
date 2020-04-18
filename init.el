(setq init-dir (file-name-directory (or load-file-name (buffer-file-name))))

(require 'package)

(defvar gnu '("gnu" . "https://elpa.gnu.org/packages/"))
(defvar melpa '("melpa" . "https://melpa.org/packages/"))
(defvar melpa-stable '("melpa-stable" . "https://stable.melpa.org/packages/"))

(setq package-archives nil)
(add-to-list 'package-archives melpa-stable t)
(add-to-list 'package-archives melpa t)
(add-to-list 'package-archives gnu t)

(package-initialize)

(unless (and (file-exists-p (concat init-dir "elpa/archives/gnu"))
             (file-exists-p (concat init-dir "elpa/archives/melpa"))
             (file-exists-p (concat init-dir "elpa/archives/melpa-stable")))
  (package-refresh-contents))

(package-install 'use-package)

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook #'enable-paredit-mode))

(use-package company
  :ensure t
  :bind (("C-c /" . company-complete))
  :config (global-company-mode))

(use-package magit
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package cider
  :ensure t
  :init
  (setq cider-lein-command "/usr/local/bin/lein")
  (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
  :config
  (add-hook 'cider-repl-mode-hook #'eldoc-mode))

(use-package clj-refactor
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'clj-refactor-mode))

(use-package treemacs
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (projectile-mode))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-on)

  (use-package perspective
    :ensure t
    :config
    (persp-mode)))

(use-package counsel
  :ensure t
  :bind
  (("M-x" . counsel-M-x)
   ("M-y" . counsel-yank-pop)
   ("C-s" . swiper)
   ("C-c C-r" . ivy-resume)
   ("M-i" . counsel-imenu))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)))

(use-package doom-themes
  :ensure t
  :init
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
   doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  (load-theme 'doom-spacegrey t)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-init))

;; Backup configuration
(let ((backup-dir (concat init-dir "backups")))
  (if (not (file-exists-p backup-dir))
      (make-directory backup-dir t))
  (setq backup-directory-alist `(("." . ,backup-dir))
        make-backup-files t
        backup-by-copying t    ; don't clobber symlinks
        version-control t      ; version numbers for backup files
        delete-old-versions t  ; delete excess backup files silently
        kept-old-versions 3    ; oldest versions to keep (default: 2)
        kept-new-versions 3    ; newest versions to keep (default: 2)
  ))

;; Auto-save configuration
(let ((auto-save-dir (concat init-dir "auto-saves")))
  (if (not (file-exists-p auto-save-dir))
      (make-directory auto-save-dir t))
  (setq auto-save-file-name-transforms `((".*" ,(concat auto-save-dir "/") t))
        auto-save-default t     ; auto-save every buffer that visits a file
        auto-save-timeout 10    ; number of seconds of idle time before auto-save (default: 30)
        auto-save-interval 100  ; number of keystrokes between auto-saves (default: 300)
  ))

;; Trash configuration
(let ((trash-dir (concat init-dir "trash")))
  (if (not (file-exists-p trash-dir))
      (make-directory trash-dir))
  (setq trash-directory trash-dir
        delete-by-moving-to-trash t))

;; Misc configuration
(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)
(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode)
(global-prettify-symbols-mode 1)
(set-face-attribute 'default nil :font "Hack-14")
(setq ring-bell-function (lambda ()))
(setq visible-bell nil)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(setq mac-option-modifier 'none)
(setq mac-command-modifier 'meta)
(setq ns-function-modifier 'hyper)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" default)))
 '(global-hl-line-mode nil)
 '(line-number-mode t)
 '(package-selected-packages
   (quote
    (treemacs arjen-grey-theme magit clj-refactor cider company which-key paredit use-package))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-minibuffer-match-face-2 ((t (:inherit ivy-minibuffer-match-face-1 :background "#ECBE7B" :foreground "#1B2229" :weight semi-bold)))))
