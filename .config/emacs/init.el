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

;; Make emacs upgrade built-in packages
(setq package-install-upgrade-built-in t)

;; setup performance tricks for LSP
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; -- LOOK AND FEEL --

;; Do away with all the unnecessary GUI stuff
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Set the basic look and feel and behavioral settings
(use-package emacs
  :config
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
	(setq catppuccin-flavor 'mocha)
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

(use-package evil-commentary
	:ensure t
	:config
	(evil-commentary-mode))

(use-package better-jumper
	:ensure t
	:after evil
	:config
	(better-jumper-mode +1)
	(with-eval-after-load 'evil-maps
		(define-key evil-motion-state-map (kbd "C-o") 'better-jumper-jump-backward)
		(define-key evil-motion-state-map (kbd "C-M-o") 'better-jumper-jump-forward)))

(use-package evil-numbers
	:ensure t
	:after evil
	:config
	(define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
	(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt))

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
	(setq org-use-property-inheritance t)
	;; Enable code block execution for python
	(org-babel-do-load-languages
	 'org-babel-load-languages
	 '((python . t)))
	(setq org-src-fontify-natively t
				org-export-latex-listings t
				org-latex-listings 'listings
				org-latex-prefer-user-labels t)
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
	(org-agenda-files #'(vulpea-project-files))
  ;; (org-agenda-files
  ;;        '("/home/arjen/stack/roam-new/20231008105247-planning.org"
  ;;          "/home/arjen/stack/roam-new/📥 Inbox.org"
  ;;          "/home/arjen/stack/roam-new/20231008105710-tickler.org"
  ;;          "/home/arjen/stack/Notebook/inbox.org"
	;; 				 ))

	;; refile targets, 3 levels deep.
  (org-refile-targets
   '((nil :maxlevel . 3)
     (org-agenda-files :maxlevel . 3))
   ;; Without this, completers like ivy/helm are only given the first level of
   ;; each outline candidates. i.e. all the candidates under the "Tasks" heading
   ;; are just "Tasks/". This is unhelpful. We want the full path to each refile
   ;; target! e.g. FILE/Tasks/heading/subheading
   org-refile-use-outline-path 'file)
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
														(file+headline "~/stack/roam-new/20231008105247-planning.org" "Inbox")
                            "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
                           ("T" "Tickler" entry
														(file+headline "~/stack/roam-new/20231008105247-planning.org" "Inbox")
                            "* %i%? \n %U")
                           ("w" "Web site" entry
                            (file "")
                            "* %a :website:\n\n%U %?\n\n%:initial")
                           ("wN" "Web link" entry
                            (file+headline ,(car org-agenda-files)
                                           "Links to read later")
                            "* TODO [#A]  %?%a \nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"Fri\"))\n"
                            :immediate-finish t :empty-lines 1)
                           ("e" "email" entry (file+headline "~/stack/roam-new/20231008105247-planning.org" "Inbox")
                            "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")))

  :bind (("C-c c" . org-capture)
				 ("C-c a" . org-agenda)
				 ("C-c s" . org-save-all-org-buffers))
  )

(use-package org-download
	:ensure t
	:config
	(setq org-download-method 'attach))

;; (let* ((variable-tuple
;; 				(cond ((x-list-fonts "iMWritingQuatNerdFont")         '(:font "iMWritingQuatNerdFont"))
;; 							((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;; 							((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;; 							((x-list-fonts "Verdana")         '(:font "Verdana"))
;; 							((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;; 							(nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;; 			 ;;(base-font-color     (face-foreground 'default nil 'default))
;; 			 ;;(headline           `(:inherit default :weight bold :foreground ,base-font-color))
;; 			 )

;;   (custom-theme-set-faces
;;    'user
;;    `(org-level-8 ((t ( ,@variable-tuple))))
;;    `(org-level-7 ((t ( ,@variable-tuple))))
;;    `(org-level-6 ((t ( ,@variable-tuple))))
;;    `(org-level-5 ((t ( ,@variable-tuple))))
;;    `(org-level-4 ((t ( ,@variable-tuple :height 1.1))))
;;    `(org-level-3 ((t ( ,@variable-tuple :height 1.25))))
;;    `(org-level-2 ((t ( ,@variable-tuple :height 1.5))))
;;    `(org-level-1 ((t ( ,@variable-tuple :height 1.75))))
;;    `(org-document-title ((t (,@variable-tuple :height 2.0 :underline nil))))))
;; (custom-theme-set-faces
;;  'user
;;  '(variable-pitch ((t (:family "iMWritingQuatNerdFont" :height 160 :weight normal))))
;;  '(fixed-pitch ((t ( :family "Fira Code Retina" :height 160)))))
;;   (add-hook 'org-mode-hook 'variable-pitch-mode)
;; (use-package org-attach
;; 	:config
;; 	(setq-default org-attach-id-dir (expand-file-name ".attach/" org-directory))
;; 	;; Add inline image previews for attachment links
;; 	(org-link-set-parameters "attachment" :image-data-fun #'+org-inline-image-data-fn)
;; 	)

;;   (custom-theme-set-faces
;;    'user
;;    '(org-block ((t (:inherit fixed-pitch))))
;;    '(org-code ((t (:inherit (shadow fixed-pitch)))))
;;    '(org-document-info ((t (:foreground "dark orange"))))
;;    '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
;;    '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
;;    '(org-link ((t (:foreground "royal blue" :underline t))))
;;    '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;;    '(org-property-value ((t (:inherit fixed-pitch))) t)
;;    '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;;    '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
;;    '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
;;    '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

(use-package org-roam
  :ensure t
	:after vulpea
  :custom
	;; show tags in org-roam finder
	(org-roam-node-display-template "${title:*} ${tags:50}")
  (org-roam-directory (file-truename "~/stack/roam-new/"))
  (org-roam-complete-everywhere t)
	(org-roam-dailies-capture-templates
   '(("d" "default" entry "** TODO %?"
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
				 ("C-c r d" . org-roam-dailies-goto-today)
         ("C-c r f" . org-roam-node-find)
         ("C-c r g" . org-roam-graph)
         ("C-c r i" . org-roam-node-insert)
         ("C-c r c" . org-roam-capture)
         ;; Dailies
         ("C-c r j" . org-roam-dailies-capture-today))
  :config
  (org-roam-setup)
	(require 'org-roam-protocol)
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

	;; I love using org-roam, it helps me structure thoughts and ideas.
	;; Up to now I had my agenda files outside of org-roam, but that disconnects
	;; projects, notes and planning. Using a write-up by Boris Buliga I was able
	;; to use my org-agenda together with org-roam. Below are the functions
	;; required to make this happen.
	
	;; Using org-roam as an efficient Org-Agenda system 
	;; https://d12frosted.io/posts/2020-06-24-task-management-with-roam-vol2.html
	(defun aw/has-todo-items-p ()
		"Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
		(org-element-map                          ; (2)
				(org-element-parse-buffer 'headline) ; (1)
				'headline
			(lambda (h)
				(eq (org-element-property :todo-type h)
						'todo))
			nil 'first-match))                     ; (3)

	(add-hook 'find-file-hook #'vulpea-project-update-tag)
	(add-hook 'before-save-hook #'vulpea-project-update-tag)

	(defun vulpea-project-update-tag ()
    "Update PROJECT tag in the current buffer."
    (when (and (not (active-minibuffer-window))
               (org-roam-buffer-p))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (vulpea-buffer-tags-get))
               (original-tags tags))
          (if (aw/has-todo-items-p)
              (setq tags (cons "planner" tags))
            (setq tags (remove "planner" tags)))

          ;; cleanup duplicates
          (setq tags (seq-uniq tags))

          ;; update tags if changed
          (when (or (seq-difference tags original-tags)
                    (seq-difference original-tags tags))
            (apply #'vulpea-buffer-tags-set tags))))))

	(defun vulpea-project-files ()
		"Return a list of note files containing 'planner' tag." ;
		(seq-uniq
		 (seq-map
			#'car
			(org-roam-db-query
			 [:select [nodes:file]
								:from tags
								:left-join nodes
								:on (= tags:node-id nodes:id)
								:where (like tag (quote "%\"planner\"%"))]))))

	(defun vulpea-agenda-files-update (&rest _)
		"Update the value of `org-agenda-files'."
		(setq org-agenda-files (vulpea-project-files)))

	(advice-add 'org-agenda :before #'vulpea-agenda-files-update)
	(advice-add 'org-todo-list :before #'vulpea-agenda-files-update)

	(setq org-agenda-prefix-format
				'((agenda . " %i %-12(vulpea-agenda-category)%?-12t% s")
					(todo . " %i %-12(vulpea-agenda-category) ")
					(tags . " %i %-12(vulpea-agenda-category) ")
					(search . " %i %-12(vulpea-agenda-category) ")))

	(defun vulpea-agenda-category ()
		"Get category of item at point for agenda.

Category is defined by one of the following items:

- CATEGORY property
- TITLE keyword
- TITLE property
- filename without directory and extension

Usage example:

  (setq org-agenda-prefix-format
        '((agenda . \" %(vulpea-agenda-category) %?-12t %12s\")))

Refer to `org-agenda-prefix-format' for more information."
		(let* ((file-name (when buffer-file-name
												(file-name-sans-extension
												 (file-name-nondirectory buffer-file-name))))
					 (title (vulpea-buffer-prop-get "title"))
					 (category (org-get-category)))
			(or (if (and
							 title
							 (string-equal category file-name))
							title
						category)
					"")))
	)

(use-package vulpea
	:ensure t)

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
	(evil-mode -1)
	(org-present-hide-cursor)
	(flyspell-mode-off)
	(git-gutter+-mode -1)
	)	

(defun my/present-end ()
	(org-present-small)
	(org-remove-inline-images)
	(evil-mode t)
	(org-present-show-cursor)
	(flyspell-mode)
	(git-gutter+-mode 1)
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
	:ensure t
	:bind (("C-c l" . org-ref-insert-link-hydra/body))
	)

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
  :bind (("C-SPC". company-complete))
	:custom
	(company-idle-delay 10)
	(company-tooltip-idle-delay 10)
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


(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l"
				lsp-inlay-hint-enable t)
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (rustic-mode . lsp)
         (go-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-java
	:ensure t
	:after lsp-mode
	:hook (
				 (java-mode . #'lsp)
				 ))

;; optionally
(use-package lsp-ui
	:ensure t
	:commands lsp-ui-mode)
;; if you are helm user
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode
	:ensure t
	)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; Program in golang

;; (use-package treesit
;; 	:custom
;; 	(treesit-font-lock-level 4)
;; 	:config
;; 	(setq treesit-language-source-alist
;; 				'((bash "https://github.com/tree-sitter/tree-sitter-bash")
;; 					(cmake "https://github.com/uyha/tree-sitter-cmake")
;; 					(css "https://github.com/tree-sitter/tree-sitter-css")
;; 					(elisp "https://github.com/Wilfred/tree-sitter-elisp")
;; 					(go "https://github.com/tree-sitter/tree-sitter-go")
;; 					(go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
;; 					(html "https://github.com/tree-sitter/tree-sitter-html")
;; 					(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
;; 					(json "https://github.com/tree-sitter/tree-sitter-json")
;; 					(make "https://github.com/alemuller/tree-sitter-make")
;; 					(markdown "https://github.com/ikatyang/tree-sitter-markdown")
;; 					(python "https://github.com/tree-sitter/tree-sitter-python")
;; 					(toml "https://github.com/tree-sitter/tree-sitter-toml")
;; 					(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
;; 					(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
;; 					(yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; 	(setq major-mode-remap-alist
;; 				'((yaml-mode . yaml-ts-mode)
;; 					(bash-mode . bash-ts-mode)
;; 					(js2-mode . js-ts-mode)
;; 					(typescript-mode . typescript-ts-mode)
;; 					(json-mode . json-ts-mode)
;; 					(css-mode . css-ts-mode)
;; 					(go-mode . go-ts-mode)
;; 					(python-mode . python-ts-mode)))
;; 	)

(use-package go-mode
  :ensure t
  :bind (("C-c d" . flymake-show-buffer-diagnostics))
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

(use-package rustic
	:ensure t)

(use-package terraform-mode
	:ensure t)

(use-package dockerfile-mode
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
	 '(elfeed lsp-java dockerfile-mode terraform-mode lsp-ui dap-mode lsp-mode racer rustic request org-download org-msg evil-commentary vulpea evil-numbers devdocs golden-ratio evil-mode smartparens-mode smartparens smart-parens neotree git-gutter-fringe+ mini-frame evil better-jumper org-roam-bibtex org-ref org-plus-contrib visual-fill-column org-present multiple-cursors imenu-list olivetti chatgpt-shell org-bullets nix-mode org-roam-ui pdf-tools undo-tree format-all doom-modeline ox-hugo marginalia projectile-ripgrep projectile nerd-icons-completion nerd-icons company-bibtex org-roam vterm-toggle vterm which-key vertico s orderless magit go-mode envrc company catppuccin-theme))
 '(safe-local-variable-values
	 '((flyspell-mode . 0)
		 (lsp-ltex-language . "nl")
		 (lsp-ltex-language . nl-NL)
		 (ispell-dictionary . "nl")
		 (lsp-ltex-language . "nl-NL")
		 (ispell-dictionary . "nl_NL"))))


;; My personal config
(let ((filename "~/.config/personal-emacs/personal.el"))
	(if (file-exists-p filename)
			(progn
				(load filename))))

;; see: https://emacs.stackexchange.com/questions/39359/tool-bar-in-emacsclient/39361#39361
(defun my-frame-tweaks (&optional frame)
  "My personal frame tweaks."
  (unless frame
    (setq frame (selected-frame)))
  (when frame
    (with-selected-frame frame
      (when (display-graphic-p)
        (tool-bar-mode -1))))
	
	(set-frame-parameter (selected-frame) 'alpha '(95 95))
	(add-to-list 'default-frame-alist '(alpha 95 95))

  (set-face-font 'default "JetbrainsMono Nerd Font-16")
	(catppuccin-reload)
	)

;; For the case that the init file runs after the frame has been created
;; Call of emacs without --daemon option.
(my-frame-tweaks)
;; For the case that the init file runs before the frame is created.
;; Call of emacs with --daemon option.
(add-hook 'after-make-frame-functions #'my-frame-tweaks t)
(add-hook 'server-after-make-frame-hook #'catppuccin-reload)

(add-to-list 'load-path "/home/arjen/.config/emacs/rascal-emacs-mode")
(setq auto-mode-alist (cons '("\\.rsc" . rascal-mode) auto-mode-alist))
(autoload 'rascal-mode "rascal-mode-cc"
  "mode for editing Rascal source files" t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


