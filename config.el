;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(after! company
  (setq company-idle-delay 0))

(def-package! lsp-mode
  :hook
  (haskell-mode . lsp)
  :config
  (require 'lsp-clients))

(def-package! lsp-ui)
(def-package! company-lsp)
(def-package! lsp-haskell
  :after haskell-mode
  :config
  (setq lsp-haskell-process-path-hie "hie-wrapper"))

(def-package! haskell-mode
  :mode "\\.hs$"
  :config
  (company-mode)
  (flycheck-mode))
