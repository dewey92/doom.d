;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(after! company
  (setq company-idle-delay 0.1))

(require 'doom-themes)
(require 'evil-multiedit)

;; ================================================================================
;; KEY BINDINGS
;; ================================================================================
(evil-multiedit-default-keybinds) ;; call to bind keybindings
(map! :n ";" 'evil-ex)

;; ================================================================================
;; GLOBAL SETTINGS
;; ================================================================================
(setq
  avy-all-windows t
  multi-term-program "/bin/zsh"
  projectile-project-search-path '("~/Projects/")
)

;; ================================================================================
;; DISPLAY SETTINGS
;; ================================================================================
(setq-default
  display-line-numbers-type 'relative
  line-spacing 5
  tab-width 2
)
(setq
  doom-font (font-spec :family "Fira Code" :size 13)
  doom-theme 'doom-horizon
  doom-themes-enable-bold t    ; if nil, bold is universally disabled
  doom-themes-enable-italic t  ; if nil, italics is universally disabled
)

(global-display-line-numbers-mode)

;; Dim when not in focus
(add-hook 'after-init-hook (lambda ()
  (when (fboundp 'auto-dim-other-buffers-mode)
    (auto-dim-other-buffers-mode t))))

;; ================================================================================
;; TABS AND SPACEES CONFIGURATION
;; ================================================================================

;; Two callable functions for enabling/disabling tabs in Emacs
(defun disable-tabs () (setq indent-tabs-mode nil))
(defun enable-tabs  ()
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  (setq tab-width 2))

(add-hook 'prog-mode-hook 'enable-tabs) ;; Eable tabs fo all files
(add-hook 'lisp-mode-hook 'disable-tabs) ;; except LISP
(add-hook 'emacs-lisp-mode-hook 'disable-tabs) ;; and Emacs-LISP

;; Show tab whitespace!!!
(global-whitespace-mode)

(setq whitespace-style '(face tabs tab-mark trailing))

(setq whitespace-display-mappings
  '((tab-mark 9 [124 9] [92 9])))

;; Delete trailing spaces on save
(add-hook 'after-save-hook #'delete-trailing-whitespace)

;; (add-hook!
;;   js2-mode 'prettier-js-mode
;;   (add-hook 'before-save-hook #'refmt-before-save nil t))

;; (def-package! tide
;;   :after (typescript-mode company flycheck)
;;   :hook ((typescript-mode . tide-setup)
;;          (typescript-mode . tide-hl-identifier-mode)
;;          (before-save . tide-formater-before-save)))

(def-package! web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (setq web-mode-enable-current-element-highlight t))

;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

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
 '(wakatime-api-key "53492fc8-ae10-438e-a016-2900c6d07f72"))
(put 'customize-group 'disabled nil)
