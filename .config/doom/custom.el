;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
<<<<<<< HEAD
   '((eval progn (setenv "DATABASE_URL" "jdbc:postgresql://localhost:5432/comics")
=======
   '((cider-clojure-cli-aliases . ":dev") (cider-shadow-default-options . "app")
     (cider-default-cljs-repl . shadow)
     (eval progn
      (setenv "DATABASE_URL" "jdbc:postgresql://localhost:5432/comics")
>>>>>>> 99a2834 (feat: clojure-lsp)
      (setenv "DATABASE_USER" "postgres") (setenv "DATABASE_PASS" "test"))
     (lsp-ltex-language . "nl") (lsp-ltex-language . nl-NL)
     (lsp-ltex-language . "nl-NL") (ispell-dictionary . "nl_NL")
     (ispell-dictionary . "nl"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
