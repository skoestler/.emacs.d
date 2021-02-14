;; Random variables that could not be configured in the UI.

(setq inhibit-startup-message t)
(setq frame-title-format nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-clojure-cli-global-options "-A:dev")
 '(clojure-align-forms-automatically t)
 '(custom-safe-themes
   (quote
    ("6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" default)))
 '(default-frame-alist
    (quote
     ((ns-appearance . dark)
      (ns-transparent-titlebar . t))))
 '(global-prettify-symbols-mode t)
 '(hl-paren-background-colors
   (quote
    ("#4C566A" "#4C566A" "#4C566A" "#4C566A" "#4C566A" "#4C566A" "#4C566A")))
 '(hl-paren-colors
   (quote
    ("#C16069" "#D2876D" "#ECCC87" "#A2BF8A" "#8EBCBB" "#80A0C2" "#5C748E" "#B58DAE" "#5D80AE" "#86C0D1" "#507681")))
 '(menu-bar-mode nil)
 '(ns-command-modifier (quote meta))
 '(package-archives
   (quote
    (("melpa-stable" . "https://stable.melpa.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/"))))
 '(package-selected-packages
   (quote
    (smartparens use-package-chords highlight-parentheses rainbow-delimiters rainbow-delimiters-mode doom-modeline aggressive-indent clj-refactor cider paredit counsel doom-themes doom-theme avy-zap avy which-key use-package)))
 '(ring-bell-function (quote ignore))
 '(scroll-bar-mode nil)
 '(smartparens-global-strict-mode t)
 '(sp-navigate-interactive-always-progress-point t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#2F3841" :foreground "#c0c5ce" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :foundry "nil" :family "Hack")))))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package projectile
  :ensure      t
  :config      (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package counsel-projectile
  :ensure t
  :config (counsel-projectile-mode))

(use-package perspective
  :ensure t
  :config (persp-mode))

(use-package company
  :ensure t
  :bind   (("C-c /" . company-complete))
  :config (global-company-mode))

(use-package avy
  :ensure t
  :bind   ("C-c ." . avy-goto-char))

(use-package ivy
  :ensure t
  :config (ivy-mode))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-nord t)
  (doom-themes-visual-bell-config))

(use-package doom-modeline
  :ensure t
  :defer  t
  :hook   (after-init . doom-modeline-init))

(use-package counsel
  :ensure t
  :config (counsel-mode))

(use-package swiper
  :ensure t
  :bind ("C-s" . swiper))

(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode clojure-mode) . paredit-mode))

(use-package highlight-parentheses
  :ensure t
  :hook   ((clojure-mode emacs-lisp-mode) . highlight-parentheses-mode))

(use-package aggressive-indent
  :ensure t
  :hook
  ((emacs-lisp-mode clojure-mode) . aggressive-indent-mode))

(use-package cider
  :ensure t
  :init
  (setq cider-lein-command
	"/usr/local/bin/lein")
  (setq cider-cljs-lein-repl
	"(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
  :hook (cider-repl-mode-hook . eldoc-mode))

(use-package clj-refactor
  :ensure t
  :hook (clojure-mode . clj-refactor-mode))

(defun clojure/fancify-symbols (mode)
  "Pretty symbols for Clojure's anonymous functions and sets,
   like (λ [a] (+ a 5)), ƒ(+ % 5), and ∈{2 4 6}."
  (font-lock-add-keywords
   mode
   `(("(\\(fn\\)[\[[:space:]]"
      (0 (progn (compose-region (match-beginning 1)
				(match-end 1) "λ"))))
     ("(\\(partial\\)[\[[:space:]]"
      (0 (progn (compose-region (match-beginning 1)
				(match-end 1) "Ƥ"))))
     ("(\\(comp\\)[\[[:space:]]"
      (0 (progn (compose-region (match-beginning 1)
				(match-end 1) "∘"))))
     ("\\(#\\)("
      (0 (progn (compose-region (match-beginning 1)
				(match-end 1) "ƒ"))))
     ("\\(#\\){"
      (0 (progn (compose-region (match-beginning 1)
				(match-end 1) "∈")))))))

(clojure/fancify-symbols 'clojure-mode)
