(setq user-full-name "Arjen Wiersma")
(setq user-mail-address "arjen@wiersma.org")

(require 'package)

(defvar gnu '("gnu" . "https://elpa.gnu.org/packages/"))
(defvar melpa '("melpa" . "https://melpa.org/packages/"))
(defvar melpa-stable '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; Add marmalade to package repos
(setq package-archives nil)
(add-to-list 'package-archives melpa-stable t)
(add-to-list 'package-archives melpa t)
(add-to-list 'package-archives gnu t)

(package-initialize)

(unless (and (file-exists-p "~/.config/emacs/elpa/archives/gnu")
             (file-exists-p "~/.config/emacs/elpa/archives/melpa")
             (file-exists-p "~/.config/emacs/elpa/archives/melpa-stable"))
  (package-refresh-contents))

;; -- LOOK AND FEEL --
(global-prettify-symbols-mode 1)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq mac-option-modifier 'none)
(setq mac-command-modifier 'meta)
(setq ns-function-modifier 'hyper)

;; http://stackoverflow.com/questions/11679700/emacs-disable-beep-when-trying-to-move-beyond-the-end-of-the-document
(defun my-bell-function ())

(setq ring-bell-function 'my-bell-function)
(setq visible-bell nil)

(use-package catppuccin-theme
  :ensure t
  :config
  (load-theme 'catppuccin t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda ()
                   (flyspell-mode 1)
                   (visual-line-mode 1)
                   )))

(use-package emacs
  :config
  (set-face-attribute 'default nil :font "JetbrainsMono Nerd Font-16")
  :custom
  (use-short-answers t)
  (inhibit-startup-screen t))

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :config
  (nerd-icons-completion-mode))

;; -- Utilities ---
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package vterm
  :ensure t)

(use-package vterm-toggle
  :ensure t
  :after vterm
  :bind (("C-c o t" . vterm-toggle-cd)
	 ("C-c o T" . vterm-toggle)
	 ))

(use-package projectile
  :ensure t)

(use-package projectile-ripgrep
  :ensure t)

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

(defun nuke-all-buffers ()
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))

(global-set-key (kbd "C-x K") 'nuke-all-buffers)

(defun get-openai-key ()
  (let ((info (nth 0 (auth-source-search
                      :host "openai.com"
                      :require '(:user :secret)
                      :create t))))
    (if info
        (let ((secret (plist-get info :secret)))
          (if (functionp secret)
              (funcall secret)
            secret))
      nil)))

(use-package chatgpt-shell
  :ensure t
  :custom
  (chatgpt-shell-openai-key (get-openai-key)))

(use-package olivetti
  :ensure t)

(use-package imenu-list
  :ensure t
  :bind (("C-c i" . imenu-list-smart-toggle))
  :custom
  (imenu-list-focus-after-activation t))

;; -- org-mode --

(use-package org
  :custom
  (truncate-lines nil)
  (org-directory "~/stack/roam-new/")
  (org-agenda-files
         '("/home/arjen/stack/roam-new/20231008105247-planning.org"
           "/home/arjen/stack/roam-new/📥 Inbox.org"
           "/home/arjen/stack/roam-new/20231008105710-tickler.org"))


  (org-refile-targets '(("/home/arjen/stack/roam-new/20231008105247-planning.org" :maxlevel . 4)
                        ("/home/arjen/stack/roam-new/20231008105710-tickler.org" :maxlevel . 2)))

  (org-id-link-to-org-use-id t)
  (org-image-actual-width 800)
  (org-log-into-drawer t)

  )

(use-package org-roam
      :ensure t
      :custom
      (org-roam-directory (file-truename "~/stack/roam-new/"))
      (org-roam-complete-everywhere t)
      :bind (("C-c r l" . org-roam-buffer-toggle)
             ("C-c r f" . org-roam-node-find)
             ("C-c r g" . org-roam-graph)
             ("C-c r i" . org-roam-node-insert)
             ("C-c r c" . org-roam-capture)
             ;; Dailies
             ("C-c r j" . org-roam-dailies-capture-today))
      :config
      (org-roam-setup)
      ;; If using org-roam-protocol
      (require 'org-roam-protocol)

      ;; (cl-defmethod org-roam-node-type ((node org-roam-node))
      ;;   "Return the TYPE of NODE."
      ;;   (condition-case nil
      ;;       (file-name-nondirectory
      ;;        (directory-file-name
      ;;         (file-name-directory
      ;;          (file-relative-name (org-roam-node-file node) org-roam-directory))))
      ;; (error "")))
      :config
      (setq org-roam-capture-templates
            '(("m"
               "main" plain
               "%?"
               :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
				  "#+title: ${title}\n")
               :immediate-finish t
               :unnarrowed t)
              ("c"
               "comics" plain "%?"
               :if-new
               (file+head "comics/%<%Y%m%d%H%M%S>-${slug}.org"
			  "#+title: ${title}\n#+filetags: :area:comics:\n")
               :unnarrowed t)
              ("p"
               "papers" plain "%?"
               :if-new
               (file+head "papers/%<%Y%m%d%H%M%S>-${citar-citekey}-${citar-date}.org"
			  "#+title: ${citar-citekey} (${citar-date}). ${note-title}.\n#+created: %U\n#+last_modified: %U\n#+filetags: :paper:bib:\n\n\n")
               :unnarrowed t)
              ("h"
               "home" plain "%?"
               :if-new
               (file+head "home/%<%Y%m%d%H%M%S>-${slug}.org"
			  "#+title: ${title}\n#+filetags: :home:\n")
               :unnarrowed t)
              ("n"
               "novi" plain "%?"
               :if-new
               (file+head "novi/%<%Y%m%d%H%M%S>-${slug}.org"
			  "#+title: ${title}\n#+filetags: :novi:\n")
               :immediate-finish t
               :unnarrowed t)
              ("s"
               "security" plain "* Background %?\n\n* Examples\n\n\n* References\n\n"
               :if-new
               (file+head "security/%<%Y%m%d%H%M%S>-${slug}.org"
			  "#+title: ${title}\n#+filetags: :security:\n")
               :immediate-finish t
               :unnarrowed t)
              ("t"
               "thesis" plain "%?"
               :if-new
               (file+head "thesis/%<%Y%m%d%H%M%S>-${slug}.org"
			  "#+title: ${title}\n#+filetags: :thesis:\n")
               :immediate-finish nil
               :unnarrowed t)
              ("o"
               "OU Study Notes" plain "%?"
               :if-new
               (file+head "study/%<%Y%m%d%H%M%S>-${slug}.org"
			  "#+title: ${title}\n#+filetags: :study:\n")
               :immediate-finish nil
               :unnarrowed t)))      
      (setq org-roam-capture-ref-templates '(("r"
                                              "ref" plain "* Notes

- ${body}%?

* Key takeaways

-

* Quotes

#+begin_quote
#+end_quote

* Summary
"
                                              :if-new
                                              (file+head "links/${slug}.org"
							 "#+title: ${title}\n#+filetags: :reading:notstarted:\n")
                                              :unnarrowed t)
                                             ("c"
                                              "collection" entry "** ${title}\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:ROAM_REFS: ${ref}\n:END:"
                                              :target (file+olp "links/collection.org" ("Inbox"))
                                              :unnarrowed t)))
      )

(use-package org-roam-ui
  :ensure t)

(use-package org-bullets
  :ensure t
  :custom
  (org-bullets-bullet-list '("❶" "❷" "❸" "❹"))
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))

(use-package citar
  :ensure t
  :bind (("C-c b" . citar-insert-citation)
         :map minibuffer-local-map
         ("M-b" . citar-insert-preset))
  :custom
  (reftex-default-bibliography "/home/arjen/stack/Studie/Open-Universiteit/My-Library.bib")
  (bibtex-completion-bibliography '("/home/arjen/stack/Studie/Open-Universiteit/My-Library.bib"))
  (citar-file-note-org-include '(org-id org-roam-ref))
  (citar-bibliography '("~/stack/Studie/Open-Universiteit/My-Library.bib"))
  (citar-notes-paths '("~/stack/roam/papers"))
  (citar-library-paths '("~/stack/Zotero/pdf"))  
  )

(use-package citar-org-roam
  :ensure t
  :after citar org-roam
  :no-require
  :config (citar-org-roam-mode))

(use-package company-bibtex
  :ensure t
  :config
  (add-to-list 'company-backends 'company-bibtex)
  :custom
  (company-bibtex-bibliography '("~/stack/Studie/Open-Universiteit/My-Library.bib")))

(use-package pdf-tools
  :ensure t)

;; -- COMPLETION --

;; Enable vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure t
  :init
  (savehist-mode))

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package recentf
  :custom
  (recentf-max-saved-items 200)
  (recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/sudo:"))
  :config
  (recentf-mode 1)
  :bind (("\C-x\ \C-r"  . recentf)))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


;; -- Programming --
(dolist (hook '(prog-mode-hook))
  (add-hook hook (lambda ()
                   (display-line-numbers-mode 1)
                   )))

(use-package company
  :ensure t
  :bind (("C-c /". company-complete))
  :config
  (global-company-mode))

(use-package envrc
  :ensure t
  :config
  (envrc-global-mode))

(use-package s
  :ensure t)

(use-package magit
  :ensure t
  :bind (("C-c m" . magit-status)))

(use-package go-mode
  :ensure t
  :bind (("C-c d" . flymake-show-buffer-diagnostics))
  :hook
  (go-mode . eglot-ensure)
  )

(use-package format-all
  :ensure t
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters '(("Go"     (goimports))))
  )

(use-package nix-mode
  :ensure t)

;; -- BLOGGING

(use-package ox-hugo
  :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(imenu-list olivetti chatgpt-shell org-bullets nix-mode org-roam-ui pdf-tools undo-tree format-all doom-modeline ox-hugo marginalia projectile-ripgrep projectile nerd-icons-completion nerd-icons company-bibtex org-roam vterm-toggle vterm which-key vertico s orderless magit go-mode envrc company catppuccin-theme))
 '(safe-local-variable-values
   '((lsp-ltex-language . "nl")
     (lsp-ltex-language . nl-NL)
     (ispell-dictionary . "nl")
     (lsp-ltex-language . "nl-NL")
     (ispell-dictionary . "nl_NL"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
