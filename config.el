;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; ================================================================================
;; MACOS FIXES
;; ================================================================================
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; ================================================================================
;; KEY BINDINGS
;; ================================================================================
(use-package! evil-multiedit
  :config
  (evil-multiedit-default-keybinds) ;; call to bind keybindings
  )

(map!
  :n ";" 'evil-ex
  :n "gsg" 'evil-avy-goto-word-0
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
  :init
  (setq
    wakatime-api-key "53492fc8-ae10-438e-a016-2900c6d07f72"
    wakatime-cli-path "/usr/local/bin/wakatime"
    wakatime-python-bin nil)
  :config
  (global-wakatime-mode)
)

;; Autocomplete
(use-package! company
  :init
  (setq company-idle-delay 0.1)
  )

;; ================================================================================
;; DISPLAY SETTINGS
;; ================================================================================
(setq-default
  display-line-numbers-type 'relative
  line-spacing 5
)
(setq
  doom-font (font-spec :family "Iosevka" :size 14)
  ;; doom-theme 'doom-moonlight
  ;; doom-themes-enable-bold t    ; if nil, bold is universally disabled
  ;; doom-themes-enable-italic t  ; if nil, italics is universally disabled
)

(use-package! color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-night t)
)

(use-package! highlight-indent-guides
  :init
  (setq
    highlight-indent-guides-responsive 'top)
)

;; Modeline
(use-package! doom-modeline
  :init
  (setq
    doom-modeline-buffer-encoding nil
    doom-modeline-buffer-file-name-style 'file-name
    doom-modeline-vcs-max-length 18)
)

;; Dim when not in focus
(add-hook 'after-init-hook (lambda ()
  (when (fboundp 'auto-dim-other-buffers-mode)
    (auto-dim-other-buffers-mode t))))

;; Move to the newly split window
(defun focus-other-window (orig-fn &rest args)
  (apply orig-fn args)
  (call-interactively 'other-window))
(advice-add 'evil-window-vsplit :around #'focus-other-window)
(advice-add 'evil-window-split :around #'focus-other-window)

;; ================================================================================
;; TABS AND SPACEES CONFIGURATION
;; ================================================================================
(setq-default
  indent-tabs-mode t
  tab-width 2)

(setq-hook! '(lisp-mode-hook emacs-lisp-mode-hook) indent-tabs-mode nil) ;; except LISP family

;; Making electric-indent behave sanely
(setq-default electric-indent-inhibit t)

;; Delete trailing spaces on save
(add-hook 'after-save-hook #'delete-trailing-whitespace)

;; ================================================================================
;; TYPESCRIPT CONFIGURATION
;; ================================================================================
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq
    flycheck-check-syntax-automatically '(save idle-change mode-enabled)
    tide-completion-ignore-case t
    tide-completion-show-source t
    tide-completion-detailed nil
    tide-hl-identifier-idle-time 0.5
    typescript-indent-level 2
    )
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  )

(defun tsx-setup-tide ()
  (interactive)
  (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (setup-tide-mode)))

(use-package! web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
)

(use-package! tide
  :config
  (map! :localleader
        :map tide-mode-map
        "re" #'tide-project-errors
        "rf" #'tide-rename-file
        ))

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'web-mode-hook #'tsx-setup-tide)

;; Setup linter
(after! flycheck
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-next-checker 'typescript-tide 'javascript-eslint 'append)
  (flycheck-add-next-checker 'tsx-tide 'javascript-eslint 'append)
  (when (string-equal "ts" (file-name-extension buffer-file-name))
      (flycheck-select-checker 'typescript-tide))
  (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (flycheck-select-checker 'tsx-tide))
  )

;; ================================================================================
;; PURESCRIPT
;; ================================================================================

;; ================================================================================
;; DO NOT EDIT
;; ================================================================================
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dim-other-buffers-face ((t (:background "gray20"))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))
 '(package-selected-packages '(wakatime-mode)))
(put 'customize-group 'disabled nil)
