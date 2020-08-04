;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; ================================================================================
;; MACOS FIXES
;; ================================================================================
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(add-to-list 'load-path "~/.emacs.d/3rd/")

(require 'hl-line+)
(hl-line-when-idle-interval 0.3)
(toggle-hl-line-when-idle 1)

;; ================================================================================
;; KEY BINDINGS
;; ================================================================================
(use-package! evil-multiedit
  :defer t
  :config
  (evil-multiedit-default-keybinds) ;; call to bind keybindings
  )

(map!
  :n ";" 'evil-ex
  :n "gsg" 'evil-avy-goto-word-0
  :n "gsl" 'evil-avy-goto-line
  (:leader
    "vx" 'er/expand-region)
  )

;; ================================================================================
;; GLOBAL SETTINGS
;; ================================================================================
(setq
  avy-all-windows t
  projectile-project-search-path '("~/Projects/")
)

;; Maximize on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Magit copy to clipboard current branch
(defun copy-current-branch-name-to-clipboard ()
  (interactive)
  (let ((branch (magit-get-current-branch)))
    (if branch
        (progn
          (kill-new branch)
          (message "Copied '%s'" branch))
      (user-error "No current branch"))))

;; Setup wakatime
(use-package! wakatime-mode
  :custom
  (wakatime-api-key "53492fc8-ae10-438e-a016-2900c6d07f72")
  (wakatime-cli-path "/usr/local/bin/wakatime")
  :config
  (global-wakatime-mode))

;; Autocomplete
(use-package! company
  :init
  (setq company-idle-delay 0.1))

;; ================================================================================
;; DISPLAY SETTINGS
;; ================================================================================
(setq-default display-line-numbers-type nil
              line-spacing 5)

(setq doom-font (font-spec :family "Iosevka" :size 14))

;; Set transparency
(set-frame-parameter (selected-frame) 'alpha '(98 98))
(add-to-list 'default-frame-alist '(alpha 98 98))

;; Zaiste theme
;; (setq doom-theme 'zaiste)
;; (custom-theme-set-faces! 'zaiste
;;   `(web-mode-current-element-highlight-face :background "#a1d1ff" :foreground "#e66300"))

;; Doom theme
(setq doom-theme 'doom-oceanic-next)
;; (setq doom-theme 'doom-snazzy)

;; Sanitynic theme
;; (use-package! color-theme-sanityinc-tomorrow
;;   :config
;;   (load-theme 'sanityinc-tomorrow-night t)
;; )

(use-package! zoom
  :custom
  (zoom-size '(0.5 . 0.618))
  :config
  (zoom-mode t))

(use-package! highlight-indent-guides
  :custom
  (highlight-indent-guides-responsive 'top))

;; Modeline
(use-package! doom-modeline
  :custom
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-file-name-style 'file-name)
  (doom-modeline-vcs-max-length 18)
)

;; Dim when not in focus
(use-package! dimmer
  :custom
  (dimmer-adjustment-mode :both)
  (dimmer-fraction 0.5)
  :config
  (dimmer-configure-company-box)
  (dimmer-configure-posframe)
  (dimmer-configure-which-key)
  (add-to-list 'dimmer-exclusion-regexp-list "^\\*Minibuf-[0-9]+\\*")
  (dimmer-mode t))

;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; ================================================================================
;; TABS AND SPACEES CONFIGURATION
;; ================================================================================
(setq-default indent-tabs-mode t
              tab-width 2)

(setq-hook! '(lisp-mode-hook emacs-lisp-mode-hook) indent-tabs-mode nil) ;; except LISP family

;; Making electric-indent behave sanely
;; (setq-default electric-indent-inhibit t)

;; Delete trailing spaces on save
(add-hook 'after-save-hook #'delete-trailing-whitespace)

;; ================================================================================
;; TYPESCRIPT CONFIGURATION
;; ================================================================================
(defun setup-tide-mode ()
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled idle-change)
        flycheck-disabled-checkers '(typescript-tslint)
        tide-completion-ignore-case t
        tide-completion-show-source t)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  (subword-mode 1)
  ;; Setup ESLint
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode)
  (flycheck-add-next-checker 'typescript-tide 'javascript-eslint 'append)
  (flycheck-add-next-checker 'tsx-tide 'javascript-eslint 'append))

(use-package! web-mode
  :mode (("\\.html?\\'" . web-mode))
  :custom
  (web-mode-enable-current-element-highlight t)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)

  (web-mode-enable-comment-annotation t)
  (web-mode-enable-comment-interpolation t))

(use-package! typescript-mode
  :custom
  (typescript-indent-level 2)
  :init
  (add-hook 'typescript-mode-hook 'setup-tide-mode))

(use-package! tide
  :after (typescript-mode company flycheck)
  :init
  (add-hook 'typescript-tsx-mode-hook 'setup-tide-mode)
  :config
  (map! :localleader
        :map tide-mode-map
        "re" #'tide-project-errors
        "rf" #'tide-rename-file
        ))

;; ================================================================================
;; PURESCRIPT
;; ================================================================================

;; ================================================================================
;; DO NOT EDIT
;; ================================================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))
 '(package-selected-packages '(wakatime-mode)))
(put 'customize-group 'disabled nil)
