;; My Emacs config, building since 2000
;; Set the most basic parameters
(setq user-full-name "Arjen Wiersma")
(setq user-mail-address "arjen@wiersma.org")

;; Load the package repositories, I like to use melpa and melpa-stable. 
(require 'package)

(defvar gnu '("gnu" . "https://elpa.gnu.org/packages/"))
(defvar melpa '("melpa" . "https://melpa.org/packages/"))
(defvar melpa-stable '("melpa-stable" . "https://stable.melpa.org/packages/"))
(defvar org-stable '("org" . "https://orgmode.org/elpa/"))
;; Add marmalade to package repos
(setq package-archives nil)
(add-to-list 'package-archives melpa-stable t)
(add-to-list 'package-archives melpa t)
(add-to-list 'package-archives gnu t)
(add-to-list 'package-archives org-stable t)

;; Ensure the package system is loaded
(package-initialize)

;; On first run, update the repository contents
(unless (and (file-exists-p "~/.config/emacs/elpa/archives/gnu")
             (file-exists-p "~/.config/emacs/elpa/archives/melpa")
             (file-exists-p "~/.config/emacs/elpa/archives/melpa-stable"))
  (package-refresh-contents))

;; Make the init-directory easily accessible as a variable
(setq init-directory (file-name-directory user-init-file))

;; -- LOOK AND FEEL --

;; Do away with all the unnecessary GUI stuff
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Set the basic look and feel and behavioral settings
(use-package emacs
  :config
  (set-face-attribute 'default nil :font "JetbrainsMono Nerd Font-16")
	;; kill unmodified buffers without warning
	(global-set-key [(control x) (k)] 'kill-this-buffer)
	;; Do not blink the cursor
	(blink-cursor-mode -1)
	
  :custom
	;; y / n instead of yes / no
  (use-short-answers t)
	;; do not show the startup screen
  (inhibit-startup-screen t)
	;; do not indent too much
  (tab-width 2)
	;; Use ^ instead of and
	(global-prettify-symbols-mode 1)

	;; Set modifier keys, especially useful on mac
	(mac-option-modifier 'none)
	(mac-command-modifier 'meta)
	(ns-function-modifier 'hyper)

	;; Saves of files go into the .saves directory
	(backup-directory-alist `(("." . "~/.saves")))
	;; Use copying to make backups instead of writing and renaming
	(backup-by-copying t)
	)

;; http://stackoverflow.com/questions/11679700/emacs-disable-beep-when-trying-to-move-beyond-the-end-of-the-document
(defun my-bell-function ())

;; Get rid of the visible bell as an alarm
(setq ring-bell-function 'my-bell-function)
(setq visible-bell nil)

;; My favorite theme, load it immediately
(use-package catppuccin-theme
  :ensure t
  :config
  (load-theme 'catppuccin t))

;; The mode-line from doom is killer, load it
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; For all text based modes, turn on spell checking and line wrapping
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda ()
                   (flyspell-mode 1)
                   (visual-line-mode 1)
                   )))

;; Add icons to Emacs for use in commands
(use-package nerd-icons
  :ensure t)

;; Sets additional annotators for various commands
(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;; Such as complextion commands
(use-package nerd-icons-completion
  :ensure t
	:after marginalia
  :config
  (nerd-icons-completion-mode)
	(add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; -- Utilities ---

;; Use modal editing with vi based editing facilities
(use-package evil
	:ensure t
	:config
	(evil-mode 1))

(use-package better-jumper
	:ensure t
	:after evil
	:config
	(better-jumper-mode +1)
	(with-eval-after-load 'evil-maps
		(define-key evil-motion-state-map (kbd "C-o") 'better-jumper-jump-backward)
		(define-key evil-motion-state-map (kbd "C-M-o") 'better-jumper-jump-forward)))

;; Help discover new key combinations while you are almost there already
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; A real terminal in Emacs.
(use-package vterm
  :ensure t)

;; Toggle it for easy access
(use-package vterm-toggle
  :ensure t
  :after vterm
  :bind (("C-c o t" . vterm-toggle-cd)
	 ("C-c o T" . vterm-toggle)
	 ))

;; Work on projects using projectile
(use-package projectile
  :ensure t)

;; and use ripgrep to find text inside the project
(use-package projectile-ripgrep
  :ensure t)

;; Keep an undo-tree for each file, persist history for access after closing/opening the file
(use-package undo-tree
  :ensure t
	:custom
	(undo-tree-history-directory-alist `(("." . ,(concat init-directory "undo-tree-hist/"))))
  :config
	(setq undo-tree-visualizer-diff t
				undo-tree-enable-undo-in-region t)
  (global-undo-tree-mode))

;; Easily kill all buffers
(defun nuke-all-buffers ()
	"Kill all the buffers automatically"
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))

(global-set-key (kbd "C-x K") 'nuke-all-buffers)

(defun get-openai-key ()
	"Retrieve the OpenAI key from the .authinfo.gpg file"
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

(defun set-openai-key ()
	"Set the OpenAI API key for use in ChatGPT-shell"
	(interactive)
	(setq chatgpt-shell-openai-key (get-openai-key)))

;; Interact with the OpenAI tooling
(use-package chatgpt-shell
  :ensure t)

;; Easily access a popup menu with all function names in the buffer
(use-package imenu-list
  :ensure t
  :bind (("C-c i" . imenu-list-smart-toggle))
  :custom
  (imenu-list-focus-after-activation t))

;; Change everything on multiple lines all at once
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
	 ("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<" . mc/mark-all-like-this))
  )

;; Automatically insert closing ) } ] etc. 
(use-package smartparens
	:ensure t
	:hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))

;; Snippets!
(use-package yasnippet
	:ensure t
	:config
	(yas-global-mode))

;; -- org-mode --

(use-package org
	:config
	;; Enable code block execution for python
	(org-babel-do-load-languages
	 'org-babel-load-languages
	 '((python . t)))

	;; Export latex given these mappings
	(with-eval-after-load 'ox-latex
		(add-to-list 'org-latex-classes
								 '("chapterbook"
									 "\\documentclass{book}"
									 ("\\chapter{%s}" . "\\chapter{%s}")
									 ("\\section{%s}" . "\\section*{%s}")
									 ("\\subsection{%s}" . "\\subsection*{%s}")
									 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
									 ("\\paragraph{%s}" . "\\paragraph*{%s}")
									 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
									 )))
  :custom
  (truncate-lines nil)
	;; do not indent the text to the heading
	(org-adapt-indentation nil)
	;; but do show it as such, but really there are no indents
	(org-startup-indented t)
	;; indent text according to structure
  (org-indent-mode t)
  (org-directory "~/stack/roam-new/")
	(org-attach-directory "~/stack/roam-new/.attach/")
  (org-agenda-files
         '("/home/arjen/stack/roam-new/20231008105247-planning.org"
           "/home/arjen/stack/roam-new/📥 Inbox.org"
           "/home/arjen/stack/roam-new/20231008105710-tickler.org"
           "/home/arjen/stack/Notebook/inbox.org"
					 ))

	;; refile targets, 3 levels deep.
  (org-refile-targets
   '((nil :maxlevel . 3)
     (org-agenda-files :maxlevel . 3))
   ;; Without this, completers like ivy/helm are only given the first level of
   ;; each outline candidates. i.e. all the candidates under the "Tasks" heading
   ;; are just "Tasks/". This is unhelpful. We want the full path to each refile
   ;; target! e.g. FILE/Tasks/heading/subheading
   org-refile-use-outline-path 'file
   org-outline-path-complete-in-steps nil)
	;; ensure we open links in the same frame
	(org-link-frame-setup
   '((vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus . org-gnus-no-new-news)
     (file . find-file)
     (wl . wl-other-frame)))
  (org-id-link-to-org-use-id t)
  (org-image-actual-width 800)
  (org-log-into-drawer t)
	(org-capture-templates '(("b" "Blog idea" entry (file+olp "~/stack/Notebook/notes.org" "Personal" "Series")
                            "* %?\n%T" :prepend t)
                           ("t" "todo" entry
                            (file+headline "~/stack/Notebook/inbox.org" "Tasks")
                            "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
                           ("T" "Tickler" entry
                            (file+headline "~/stack/Notebook/tickler.org" "Tickler")
                            "* %i%? \n %U")
                           ("w" "Web site" entry
                            (file "")
                            "* %a :website:\n\n%U %?\n\n%:initial")
                           ("wN" "Web link" entry
                            (file+headline ,(car org-agenda-files)
                                           "Links to read later")
                            "* TODO [#A]  %?%a \nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"Fri\"))\n"
                            :immediate-finish t :empty-lines 1)
                           ("e" "email" entry (file+headline "~/stack/Notebook/inbox.org" "Tasks from Email")
                            "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")))	
  )

(use-package org-attach
	:config
	(setq-default org-attach-id-dir (expand-file-name ".attach/" org-directory))
	;; Add inline image previews for attachment links
  (org-link-set-parameters "attachment" :image-data-fun #'+org-inline-image-data-fn)
	)

(use-package org-roam
      :ensure t
      :custom
			;; show tags in org-roam finder
			(org-roam-node-display-template "${title:*} ${tags:50}")
      (org-roam-directory (file-truename "~/stack/roam-new/"))
      (org-roam-complete-everywhere t)
			(org-roam-dailies-capture-templates
        '(("d" "default" entry "* TODO %?"
           :target (file+head "%<%Y>/%<%Y-%m-%d>.org" "# -*- ispell-dictionary: \"nl_NL\" -*-
#+TITLE: %<%B %d, %Y>
#+filetags: dailies

- tags :: [[id:6b2b4539-b6c0-4966-ae41-ff9048be1e86][Daily Notes]]

* 📅 Dagelijkse vragen

** 🌙 Gisteravond heb ik...

** 🚀 Vandaag wil ik bereiken...

** 👏 Iets waar ik naar uit kijk...

** 👎 Hier worstel ik momenteel mee...

* Routine

- [ ] Plan de dag
- [ ] Werken aan mijn thesis
- [ ] De dag afsluiten, geen open taken

* Captured items
"))))
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
  :config
  (setq org-bullets-bullet-list '("↦" "↳" "↳" "↳" "↳""↳""↳""↳"))
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))

(use-package visual-fill-column
	:ensure t
	:custom
	(visual-fill-column-width 110)
	(visual-fill-column-center-text t)
	:hook
	(org-mode . visual-fill-column-mode)
	)

(defun my/present-start ()
	(org-present-big)
	(org-display-inline-images)
	(visual-fill-column-mode 1)
	(visual-line-mode 1)
	;; (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
	;; 																	 (header-line (:height 4.0) variable-pitch)
	;; 																	 (org-document-title (:height 1.75) org-document-title)
	;; 																	 (org-code (:height 1.2) org-code)
	;; 																	 (org-verbatim (:height 1.2) org-verbatim)
	;; 																	 (org-block (:height 1.25) org-block)
	;; 																	 (org-block-begin-line (:height 0.7) org-block)))
	
	(org-present-hide-cursor)
	;;(org-present-read-only)
	(flyspell-mode-off)
	)	

(defun my/present-end ()
	(org-present-small)
	(org-remove-inline-images)
	(visual-fill-column-mode 0)
	(visual-line-mode 0)
	;; (setq-local face-remapping-alist '((default variable-pitch default)))
	(org-present-show-cursor)
	;;(org-present-read-write)
	(flyspell-mode)
	)

(use-package org-present
	:ensure t
	:after visual-fill-column
	:hook
	(org-present-mode . my/present-start)
  (org-present-mode-quit . my/present-end)
	)

(use-package citar
  :ensure t
  :bind (("C-c b" . org-cite-insert)
         :map minibuffer-local-map
         ("M-b" . citar-insert-preset))
	:config
	(defvar citar-indicator-files-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-file_o"
              :face 'nerd-icons-green
              :v-adjust -0.1)
     :function #'citar-has-files
     :padding "  " ; need this because the default padding is too low for these icons
     :tag "has:files"))
  (defvar citar-indicator-links-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-link"
              :face 'nerd-icons-orange
              :v-adjust 0.01)
     :function #'citar-has-links
     :padding "  "
     :tag "has:links"))
  (defvar citar-indicator-notes-icons
    (citar-indicator-create
     :symbol (nerd-icons-codicon
              "nf-cod-note"
              :face 'nerd-icons-blue
              :v-adjust -0.3)
     :function #'citar-has-notes
     :padding "    "
     :tag "has:notes"))
  (defvar citar-indicator-cited-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-circle_o"
              :face 'nerd-icon-green)
     :function #'citar-is-cited
     :padding "  "
     :tag "is:cited"))
  (setq citar-indicators
        (list citar-indicator-files-icons
              citar-indicator-links-icons
              citar-indicator-notes-icons
              citar-indicator-cited-icons))
	:custom
	(reftex-default-bibliography "/home/arjen/stack/Studie/Open-Universiteit/My-Library.bib")
	(bibtex-completion-bibliography '("/home/arjen/stack/Studie/Open-Universiteit/My-Library.bib"))
	(citar-bibliography '("~/stack/Studie/Open-Universiteit/My-Library.bib"))
	(org-cite-global-bibliography '("~/stack/Studie/Open-Universiteit/My-Library.bib"))
  (citar-file-note-org-include '(org-id org-roam-ref))
  (citar-notes-paths '("~/stack/roam/papers"))
  (citar-library-paths '("~/stack/Zotero/pdf"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
	(citar-indicators (list citar-indicator-files citar-indicator-notes))
  )
	
(use-package citar-org-roam
  :ensure t
  :after citar org-roam
  :no-require
  :config (citar-org-roam-mode))

(use-package org-ref
	:ensure t)

(use-package org-roam-bibtex
	:ensure t
  :after org-roam
  :config
  (require 'org-ref)) ; optional: if using Org-ref v2 or v3 citation links

(use-package company-bibtex
  :ensure t
  :config
  (add-to-list 'company-backends 'company-bibtex)
  :custom
  (company-bibtex-bibliography '("~/stack/Studie/Open-Universiteit/My-Library.bib")))

(use-package pdf-tools
  :ensure t
	:config
	(pdf-loader-install))

;; -- COMPLETION --

;; Enable vertico for completion
(use-package vertico
  :ensure t
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize nil)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure t
  :init
  (savehist-mode))

;; recent files list
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
  (setq enable-recursive-minibuffers t)
	(setq ispell-program-name "hunspell"))

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

(use-package devdocs
	:ensure t)

(use-package company
  :ensure t
  :bind (("C-c /". company-complete))
  :config
  (global-company-mode))

;; Enable the use of direnv for local directory instructions
(use-package envrc
  :ensure t
  :config
  (envrc-global-mode))

;; string package needed for magit
(use-package s
  :ensure t)

;; Git in Emacs, yay
(use-package magit
  :ensure t
	:after s
  :bind (("C-c m" . magit-status)))

;; Show changes to the buffer in the frings
(use-package git-gutter-fringe+
	:ensure t
	:hook
	((prog-mode . git-gutter+-mode)
	 (org-mode . git-gutter+-mode))
	)

;; LSP for Emacs, lightweight
(use-package eglot
	:bind (:map prog-mode-map
				 ("C-c e a" . eglot-code-actions)))

;; Program in golang
(use-package go-mode
  :ensure t
  :bind (("C-c d" . flymake-show-buffer-diagnostics))
  :hook
  (go-mode . eglot-ensure)
  )

;; Enable formatting on save automatically
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

;; -- Nursery projects
;; git clone git@github.com:chrisbarrett/nursery.git nursery
(add-to-list 'load-path "~/.config/emacs/nursery/lisp")
(use-package org-roam-dblocks
  :hook (org-mode . org-roam-dblocks-autoupdate-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 '("0527c20293f587f79fc1544a2472c8171abcc0fa767074a0d3ebac74793ab117" default))
 '(org-attach-id-dir "~/stack/roam-new/.attach/" nil nil "Customized with use-package org")
 '(package-selected-packages
	 '(devdocs golden-ratio evil-mode smartparens-mode smartparens smart-parens neotree git-gutter-fringe+ mini-frame evil better-jumper org-roam-bibtex org-ref org-plus-contrib visual-fill-column org-present multiple-cursors imenu-list olivetti chatgpt-shell org-bullets nix-mode org-roam-ui pdf-tools undo-tree format-all doom-modeline ox-hugo marginalia projectile-ripgrep projectile nerd-icons-completion nerd-icons company-bibtex org-roam vterm-toggle vterm which-key vertico s orderless magit go-mode envrc company catppuccin-theme))
 '(safe-local-variable-values
	 '((flyspell-mode . 0)
		 (lsp-ltex-language . "nl")
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

;; My personal config
(let ((filename "~/.config/personal-emacs/personal.el"))
	(if (file-exists-p filename)
			(progn
			 (load filename))))
