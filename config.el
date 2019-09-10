;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(after! company
  (setq company-idle-delay 0.1))

(require 'doom-themes)
(require 'evil-multiedit)

(evil-multiedit-default-keybinds) ;; call to bind keybindings

;; Global settings (defaults)
(setq
 doom-themes-enable-bold t    ; if nil, bold is universally disabled
 doom-themes-enable-italic t  ; if nil, italics is universally disabled
 multi-term-program "/bin/zsh"
 display-line-numbers-type 'relative
 line-spacing 3
 avy-all-windows t
 )

;; (add-hook!
;;   js2-mode 'prettier-js-mode
;;   (add-hook 'before-save-hook #'refmt-before-save nil t))

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
(load-theme 'doom-spacegrey t)

;; (def-package! tide
;;   :after (typescript-mode company flycheck)
;;   :hook ((typescript-mode . tide-setup)
;;          (typescript-mode . tide-hl-identifier-mode)
;;          (before-save . tide-formater-before-save)))

;; (def-package! web-mode
;;   :init
;;   (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
;;   (setq web-mode-enable-current-element-highlight t))
