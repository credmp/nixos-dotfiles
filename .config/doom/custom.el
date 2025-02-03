;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((cider-clojure-cli-aliases . ":otel") (cider-shadow-default-options . "app")
     (cider-default-cljs-repl . shadow) (lsp-ltex-language . "nl")
     (lsp-ltex-language . nl-NL) (lsp-ltex-language . "nl-NL")
     (ispell-dictionary . "nl_NL") (ispell-dictionary . "nl")
     (eval progn
      (setenv "DATABASE_URL" "jdbc:postgresql://localhost:5432/links")
      (setenv "DATABASE_USER" "links-user")
      (setenv "DATABASE_PASS" "links-pass")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
