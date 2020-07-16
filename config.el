;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(after! company
  (setq company-idle-delay 0.1))

(require 'color-theme-sanityinc-tomorrow)
(require 'evil-multiedit)

;; ================================================================================
;; MACOS FIXES
;; ================================================================================
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; ================================================================================
;; KEY BINDINGS
;; ================================================================================
(evil-multiedit-default-keybinds) ;; call to bind keybindings
(map!
  :n ";" 'evil-ex
  :n "g s l" 'evil-avy-goto-line)

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
(global-wakatime-mode)

;; ================================================================================
;; DISPLAY SETTINGS
;; ================================================================================
(setq-default
  display-line-numbers-type 'relative
  line-spacing 5
)
(setq
  doom-font (font-spec :family "Iosevka" :size 14)
  doom-theme 'doom-moonlight
  doom-themes-enable-bold t    ; if nil, bold is universally disabled
  doom-themes-enable-italic t  ; if nil, italics is universally disabled
)
(load-theme 'sanityinc-tomorrow-night t)

;; Modeline
(setq
 doom-modeline-buffer-encoding nil)

;; Dim when not in focus
(add-hook 'after-init-hook (lambda ()
  (when (fboundp 'auto-dim-other-buffers-mode)
    (auto-dim-other-buffers-mode t))))

;; Configure tabbar
;; (setq
;;   centaur-tabs-style "wave"
;;   centaur-tabs-height 32
;;   centaur-tabs-set-bar 'over)

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

;; (setq
;;   whitespace-style '(face tabs tab-mark trailing indentation)
;;   whitespace-display-mappings '(
;;     (tab-mark 9 [8614 9] [92 9]))
;;   )

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
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(defun tsx-setup-tide ()
  (interactive)
  (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (setup-tide-mode)))

(use-package! web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (setq tide-hl-identifier-idle-time 0.2)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq typescript-indent-level 2))

(add-hook 'web-mode-hook #'tsx-setup-tide)

;; enable typescript-tslint checker
;; (flycheck-add-mode 'typescript-tslint 'web-mode)

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
 '(whitespace-tab ((t (:background "#232530" :foreground "#636363")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))
 '(wakatime-api-key "53492fc8-ae10-438e-a016-2900c6d07f72")
 '(wakatime-cli-path "/usr/local/bin/wakatime")
 '(wakatime-python-bin nil))
(put 'customize-group 'disabled nil)
