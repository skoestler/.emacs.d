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
 '(clojure-align-forms-automatically t)
 '(custom-safe-themes
   (quote
    ("6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" default)))
 '(default-frame-alist
    (quote
     ((ns-appearance . dark)
      (ns-transparent-titlebar . t))))
 '(global-prettify-symbols-mode t)
 '(haskell-hoogle-command "~/.cabal/bin/hoogle")
 '(haskell-mode-stylish-haskell-path "~/.cabal/bin/stylish-haskell")
 '(haskell-stylish-on-save t)
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
    (haskell-mode smartparens use-package-chords highlight-parentheses rainbow-delimiters rainbow-delimiters-mode doom-modeline aggressive-indent clj-refactor cider paredit counsel doom-themes doom-theme avy-zap avy which-key use-package)))
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
 '(default ((t (:inherit nil :stipple nil :background "#2F3841" :foreground "#c0c5ce" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :foundry "nil" :family "Hack")))))

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
				(match-end 1) "λ"))))
     ("\\(#\\){"
      (0 (progn (compose-region (match-beginning 1)
				(match-end 1) "∈")))))))

(clojure/fancify-symbols 'clojure-mode)

(defun haskell-font-lock-dot-is-not-composition (start)
  "Return non-nil if the \".\" at START is not a composition operator.
This is the case if the \".\" is part of a \"forall <tvar> . <type>\"."
  (save-excursion
    (goto-char start)
    (or (re-search-backward "\\<forall\\>[^.\"]*\\="
                            (line-beginning-position) t)
        (not (or
              (string= " " (string (char-after start)))
              (null (char-before start))
              (string= " " (string (char-before start))))))))

(use-package haskell-mode
  :ensure t
  :hook
  (haskell-mode . interactive-haskell-mode)
  (haskell-mode . (lambda ()
		    (push '("\\" . ?λ) prettify-symbols-alist)
		    (push '("." ?∘ haskell-font-lock-dot-is-not-composition) prettify-symbols-alist)
		    ))
  ;;(haskell-mode . fira-code-mode)
  )

(defun fira-code-mode--make-alist (list)
  "Generate prettify-symbols alist from LIST."
  (let ((idx -1))
    (mapcar
     (lambda (s)
       (setq idx (1+ idx))
       (let* ((code (+ #Xe100 idx))
              (width (string-width s))
              (prefix ())
              (suffix '(?\s (Br . Br)))
              (n 1))
	 (while (< n width)
	   (setq prefix (append prefix '(?\s (Br . Bl))))
	   (setq n (1+ n)))
	 (cons s (append prefix suffix (list (decode-char 'ucs code))))))
     list)))

(defconst fira-code-mode--ligatures
  '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
    "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
    "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
    "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
    ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
    "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
    "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
    "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
    ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
    "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
    "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
    "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"
    "x" ":" "+" "+" "*"))

(defvar fira-code-mode--old-prettify-alist)

(defun fira-code-mode--enable ()
  "Enable Fira Code ligatures in current buffer."
  (setq-local fira-code-mode--old-prettify-alist prettify-symbols-alist)
  (setq-local prettify-symbols-alist (append (fira-code-mode--make-alist fira-code-mode--ligatures) fira-code-mode--old-prettify-alist))
  (prettify-symbols-mode t))

(defun fira-code-mode--disable ()
  "Disable Fira Code ligatures in current buffer."
  (setq-local prettify-symbols-alist fira-code-mode--old-prettify-alist)
  (prettify-symbols-mode -1))

(define-minor-mode fira-code-mode
  "Fira Code ligatures minor mode"
  :lighter " Fira Code"
  (setq-local prettify-symbols-unprettify-at-point 'right-edge)
  (if fira-code-mode
      (fira-code-mode--enable)
    (fira-code-mode--disable)))

(defun fira-code-mode--setup ()
  "Setup Fira Code Symbols"
  (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol"))

(provide 'fira-code-mode)
