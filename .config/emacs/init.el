;; My Emacs config, building since 2000
;; Set the most basic parameters
(setq user-full-name "Arjen Wiersma")
(setq user-mail-address "arjen@wiersma.org")

(setq storage "~/hetzner")

;; Load the package repositories, I like to use melpa and melpa-stable. 
(require 'package)

(defvar gnu '("gnu" . "https://elpa.gnu.org/packages/"))
(defvar melpa '("melpa" . "https://melpa.org/packages/"))
(defvar melpa-stable '("melpa-stable" . "https://stable.melpa.org/packages/"))
(defvar org-stable '("org" . "https://orgmode.org/elpa/"))

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
         (file-exists-p "~/.config/emacs/elpa/archives/melpa-stable")
         (file-exists-p "~/.config/emacs/elpa/archives/org"))
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
  (global-set-key [(control x) (k)] 'kill-buffer)
  ;;(global-set-key ["C-SPC"] 'set-mark-command) 
  ;; Do not blink the cursor
  (blink-cursor-mode -1)
  (setq-default indent-tabs-mode nil)

  :custom
  ;; y / n instead of yes / no
  (use-short-answers t)
  ;; do not show the startup screen
  (inhibit-startup-screen t)
  ;; do not indent too much
  (tab-width 4)
  ;; Use ^ instead of and
  (global-prettify-symbols-mode 1)

  ;; Set modifier keys, especially useful on mac
  (mac-option-modifier 'none)
  (mac-command-modifier 'meta)
  (ns-function-modifier 'hyper)

  ;; Saves of files go into the .saves directory
  (backup-directory-alist `(("." . "~/.saves")))
  ;; Use copying to make backups instead of writing and renaming
  (backup-by-copying t))


;; http://stackoverflow.com/questions/11679700/emacs-disable-beep-when-trying-to-move-beyond-the-end-of-the-document
(defun my-bell-function ())

;; Get rid of the visible bell as an alarm
(setq ring-bell-function 'my-bell-function)
(setq visible-bell nil)

;; My favorite theme, load it immediately
(use-package catppuccin-theme
  :ensure t
  :config
  (setq catppuccin-flavor 'macchiato)
;;  (load-theme 'catppuccin t))
  )

;;; For packaged versions which must use `require'.
(use-package modus-themes
  :ensure t
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil)

  ;; Maybe define some palette overrides, such as by using our presets
  (setq modus-themes-common-palette-overrides
        modus-themes-preset-overrides-intense)

  ;; Load the theme of your choice.
  (load-theme 'modus-operandi t)

  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

;; Give my modeline and buffer a little bit of extra padding
(use-package spacious-padding
  :ensure t
  :config
  (spacious-padding-mode))

;; The mode-line from doom is killer, load it
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; For all text based modes, turn on spell checking and line wrapping
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda ()
                   (visual-line-mode 1))))
                   

;; Keep an undo-tree for each file, persist history for access after closing/opening the file
;; (use-package undo-tree
;;   :ensure t
;;   :custom
;;   (undo-tree-history-directory-alist `(("." . ,(concat init-directory "undo-tree-hist/"))))
;;   :config
;;   (setq undo-tree-visualizer-diff t
;;         undo-tree-enable-undo-in-region t)
;;   (global-undo-tree-mode))

;; Center emacs on large screen
(use-package olivetti
  :ensure t
  :hook org-mode
  :config
  (setq olivetti-body-width 96))

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
         ("C-c o T" . vterm-toggle)))
         

;; Work on projects using projectile
(use-package projectile
  :ensure t)

;; and use ripgrep to find text inside the project
(use-package projectile-ripgrep
  :ensure t)

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
         ("C-c C-<" . mc/mark-all-like-this)))

;; Automatically to insert closing ) } ] etc. 
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

(use-package doom-snippets
  :load-path "/home/arjen/.config/emacs/doom-snippets"
  :after yasnippet)

;; -- org-mode --

(use-package org
  :config
  (defun aw/gather-agenda-files ()
    (denote-directory-files "_planning"))

  (defun aw/denote-project-update-tag ()
    "Update PROJECT tag in the current buffer."
    (when (and
           (not (active-minibuffer-window))
           (aw/org-buffer-p))
      (if (aw/has-todo-items-p) ;; it has todo items
          (if (not (seq-contains (vulpea-buffer-tags-get) "planning")) ;; but not yet the planning tag
            (vulpea-buffer-tags-add "planning"))  ;; add it
          (if (seq-contains (vulpea-buffer-tags-get) "planning") ;; else (no planning items)
            (vulpea-buffer-tags-remove "planning")))
      (denote-rename-file-using-front-matter (buffer-file-name))))

  ;; (add-hook 'find-file-hook #'aw/denote-project-update-tag)
  (add-hook 'before-save-hook #'aw/denote-project-update-tag)
  
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
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
                   
  (setq org-agenda-custom-commands
        '(
          ("x" "My Agenda"
           (
            (agenda "" (
                        ;; https://emacs.stackexchange.com/questions/38742/implement-scheduling-as-suggested-in-deep-work-using-emacs-org-mode
                        (org-agenda-sorting-strategy '((agenda habit-down time-up ts-up
                                                        priority-down category-keep)
                                                       (todo priority-down category-keep)
                                                       (tags priority-down category-keep)
                                                       (search category-keep)))

                        (org-agenda-skip-scheduled-if-done nil)
                        (org-agenda-time-leading-zero t)
                        (org-agenda-timegrid-use-ampm nil)
                        (org-agenda-skip-timestamp-if-done t)
                        (org-agenda-skip-deadline-if-done t)
                        (org-agenda-start-day "+0d")
                        (org-agenda-span 5)
                        (org-agenda-overriding-header "⚡ Calendar")
                        (org-agenda-repeating-timestamp-show-all nil)
                        (org-agenda-remove-tags t)
                        (org-agenda-prefix-format "   %i %?-2 t%s")
                        ;; (org-agenda-prefix-format "  %-3i  %-15b%t %s")
                        ;; (concat "  %-3i  %-15b %t%s" org-agenda-hidden-separator))
                        ;; (org-agenda-todo-keyword-format " ☐ ")
                        ;; (org-agenda-todo-keyword-format "%-12s")
                        (org-agenda-todo-keyword-format "")
                        (org-agenda-time)
                        (org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈ Now")
                        (org-agenda-scheduled-leaders '("" ""))
                        (org-agenda-deadline-leaders '("Deadline:  " "In %3d d.: " "%2d d. ago: "))
                        (org-agenda-time-grid (quote ((today require-timed remove-match) () "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈")))))

            (tags "+TODO=\"TODO\"" (
                                    (org-agenda-overriding-header "\n⚡ To Do")
                                    (org-agenda-sorting-strategy '(priority-down))
                                    (org-agenda-remove-tags t)
                                    ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
                                    (org-agenda-todo-ignore-scheduled 'all)
                                    (org-agenda-prefix-format "   %-2i %?b")
                                    (org-agenda-todo-keyword-format "")))

            (tags "+TODO=\"NEXT\"" (
                                    (org-agenda-overriding-header "\n⚡ Next")
                                    (org-agenda-sorting-strategy '(priority-down))
                                    (org-agenda-remove-tags t)
                                    (org-agenda-todo-ignore-scheduled 'all)
                                    (org-agenda-prefix-format "   %-2i %?b")
                                    (org-agenda-todo-keyword-format "")))))))  
  :custom
  (truncate-lines nil)
  ;; do not indent the text to the heading
  (org-adapt-indentation nil)
  ;; Note: these 2 were switched off for org-margin
  ;; but do show it as such, but really there are no indents
  ;;(org-startup-indented t)
  ;; indent text according to structure
  ;;(org-indent-mode t)
  (org-directory (concat storage "/roam-new"))
  (org-attach-directory (concat storage "/.attach"))
  ;; populate org-agenda-files using vulpea, which queries org-roam
  (org-agenda-files #'(vulpea-project-files))

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
  (org-capture-templates '(("b" "Blog idea" entry (file+olp (concat storage "/Notebook/notes.org")  "Personal" "Series")
                            "* %?\n%T" :prepend t)
                           ("t" "todo" entry
                            (file+headline (concat storage "/roam-new/20231008105247-planning.org") "Inbox")
                            "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
                           ("T" "Tickler" entry
                            (file+headline (concat storage "/roam-new/20231008105247-planning.org") "Inbox")
                            "* %i%? \n %U")
                           ("w" "Web site" entry
                            (file "")
                            "* %a :website:\n\n%U %?\n\n%:initial")
                           ("wN" "Web link" entry
                            (file+headline ,(car org-agenda-files)
                                           "Links to read later")
                            "* TODO [#A]  %?%a \nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"Fri\"))\n"
                            :immediate-finish t :empty-lines 1)
                           ("e" "email" entry (file+headline (concat storage "/roam-new/20231008105247-planning.org") "Inbox")
                            "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")))

  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c s" . org-save-all-org-buffers)))
  

;;; ORG-MODE:  * My Task
;;;              SCHEDULED: <%%(diary-last-day-of-month date)>
;;; DIARY:  %%(diary-last-day-of-month date) Last Day of the Month
;;; See also:  (setq org-agenda-include-diary t)
;;; (diary-last-day-of-month '(2 28 2017))
(defun diary-last-day-of-month (date)
  "Return `t` if DATE is the last day of the month."
  (let* ((day (calendar-extract-day date))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (last-day-of-month
          (calendar-last-day-of-month month year)))
    (= day last-day-of-month)))

(use-package org-download
  :ensure t
  :config
  (setq org-download-method 'attach))

(use-package all-the-icons
  :ensure t)



(defun aw/org-buffer-p (&optional buffer)
  "Return t if BUFFER is for an Org file.
If BUFFER is not specified, use the current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (derived-mode-p 'org-mode))))
      

(use-package org-roam
  :ensure t
  :after vulpea
  :custom
  ;; show tags in org-roam finder
  (org-roam-node-display-template "${title:*} ${tags:50}")
  (org-roam-directory (file-truename (concat storage "/roam-new/")))
  (org-roam-complete-everywhere t)
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "** TODO %?"
      :target (file+head "%<%Y>/%<%Y-%m-%d>.org" "# -*- ispell-dictionary: \"nl_NL\" -*-
#+TITLE: %<%B %d, %Y>
#+filetags: dailies

- tags :: [[id:6b2b4539-b6c0-4966-ae41-ff9048be1e86][Daily Notes]]

* 📅 Dagelijkse vragen

** 🎯 Vandaag wil ik bereiken...

** 👏 Iets waar ik naar uit kijk...

** 👎 Hier worstel ik momenteel mee...

* 💪 Reflectie op de dag

* 🌱 Daily Habits

- [ ] Plan de dag
- [ ] Lees een boek
- [ ] Reflecteer op de dag
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
  (org-roam-db-autosync-mode)
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
                      "#+title: ${citar-citekey} (${citar-date}). ${title}.\n#+created: %U\n#+last_modified: %U\n#+filetags: :paper:bib:\n\n\n")
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
           "tsjoch" plain "%?"
           :if-new
           (file+head "tsjoch/%<%Y%m%d%H%M%S>-${slug}.org"
                      "#+title: ${title}\n#+filetags: :substack:\n")
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
                                          (file+head "links/%<%Y%m%d%H%M%S>-${slug}.org"
                                                     "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n#+filetags: :reading:notstarted:\n")
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

  (defun org-agenda-open-hook ()
   (olivetti-mode))

  (add-hook 'org-agenda-mode-hook 'org-agenda-open-hook)

  (setq org-agenda-span 1
    org-agenda-start-day "+0d"
    org-agenda-skip-timestamp-if-done t
    org-agenda-skip-deadline-if-done t
    org-agenda-skip-scheduled-if-done t
    org-agenda-skip-scheduled-if-deadline-is-shown t
    org-agenda-skip-timestamp-if-deadline-is-shown t
    org-agenda-time-grid '((daily) () "" "")
    org-agenda-hide-tags-regexp ".*"
    org-agenda-prefix-format '((agenda . " %?-2i %t ")
                               (todo . " %-12:c")
                               (tags . " %-12:c")
                               (search . " %i %-12:c"))
    ;; https://fontawesome.com/v4/icons/
    org-agenda-category-icon-alist `(("Planning", (list (all-the-icons-faicon "clipboard" :heigh 0.8)) nil nil :ascent center)
                                     ("work", (list (all-the-icons-faicon "graduation-cap" :heigh 0.8)) nil nil :ascent center)
                                     ("afstuderen", (list (all-the-icons-faicon "user" :heigh 0.8)) nil nil :ascent center)
                                     ("tickler", (list (all-the-icons-faicon "clock-o" :heigh 0.8)) nil nil :ascent center)))
    
  
  ;; Using org-roam as an efficient Org-Agenda system
  ;; https://d12frosted.io/posts/2020-06-24-task-management-with-roam-vol2.html
  (defun aw/has-todo-items-p ()
    "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
    (org-element-map                         
        (org-element-parse-buffer 'headline) 
        'headline
      (lambda (h)
        (eq (org-element-property :todo-type h)
            'todo))
      nil 'first-match))                     

  ;;(add-hook 'find-file-hook #'vulpea-project-update-tag)
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
    (append
      (seq-uniq
       (seq-map
        #'car
        (org-roam-db-query
         [:select [nodes:file]
          :from tags
          :left-join nodes
          :on (= tags:node-id nodes:id)
          :where (like tag (quote "%\"planner\"%"))])))
      (aw/gather-agenda-files))) ;; depends on the function from org definition above

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
          ""))))
  

(use-package vulpea
  :ensure t)

(use-package org-roam-ui
  :ensure t)

;; (use-package org-bullets
;;   :ensure t
;;   :config
;;   ;;(setq org-bullets-bullet-list '("↦" "↳" "↳" "↳" "↳""↳""↳""↳"))
;;   ;;(setq org-bullets-bullet-list '("H1" "H2" "H3" "H4" "H5" "H6" "H7" "H8"))
;;   (setq org-bullets-bullet-list '("◉" "○" "✸" "✿"))
;;   :hook
;;   (org-mode . (lambda () (org-bullets-mode 1))))

;; (use-package org-margin
;;   :after org
;;   :load-path "org-margin"
;;   :hook org-mode)
 
(use-package org-modern
  :ensure t
  :after org
  :hook org-mode)

(use-package org-noter
  :ensure t)

(use-package org-super-agenda
  :ensure t
  :after org
  :config
  (setq org-agenda-span 1
        org-agenda-start-day "+0d"
        org-agenda-skip-timestamp-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-timestamp-if-deadline-is-shown t
        org-agenda-time-grid '((daily) () "" "")
        org-agenda-hide-tags-regexp ".*"
        org-agenda-prefix-format '((agenda . " %?-2i %t ")
                                   (todo . " %-12:c")
                                   (tags . " %-12:c")
                                   (search . " %i %-12:c"))
        ;; https://fontawesome.com/v4/icons/
        org-agenda-category-icon-alist `(("Planning", (list (all-the-icons-faicon "clipboard" :heigh 0.8)) nil nil :ascent center)
                                         ("work", (list (all-the-icons-faicon "graduation-cap" :heigh 0.8)) nil nil :ascent center)
                                         ("afstuderen", (list (all-the-icons-faicon "user" :heigh 0.8)) nil nil :ascent center)
                                         ("tickler", (list (all-the-icons-faicon "clock-o" :heigh 0.8)) nil nil :ascent center)))
          
  (org-super-agenda-mode t)
  (setq org-super-agenda-groups
      '(;; Each group has an implicit boolean OR operator between its selectors.
        (:name " Overdue "  ; Optionally specify section name
         :scheduled past
         :order 2
         :face 'error)

        (:name "Dev "
               :and(:category "development" :not (:tag "event"))
               :order 3)

        (:name "NOVI "
               :and(:category "Work" :not (:tag "event"))
               :order 3)

        (:name "Recurring "
         :and(:category "tickler" :not (:tag "event"))
         :order 3)

        (:name "Planning "
         :and(:category "Planning" :not (:tag "event"))
         :order 3)

        (:name "Meetup "
         :and(:category "Meetup" :not (:tag "event"))
         :order 3)

        (:name "Sort these out (inbox) "
         :and(:category "Inbox" :not (:tag "event"))
         :order 3)

        (:name "Personal "
         :and(:category "Personal" :not (:tag "event"))
         :order 3)

        (:name "Afstudeer studenten "
         :and(:category "afstuderen" :not (:tag "event"))
         :order 3)

        (:name " Today "  ; Optionally specify section name
         :time-grid t
         :date today
         :scheduled today
         :order 1
         :face 'warning))))
  
  
  

;; (use-package visual-fill-column
;;   :ensure t
;;   :custom
;;   (visual-fill-column-width 110)
;;   (visual-fill-column-center-text t)
;;   :hook
;;   (org-mode . visual-fill-column-mode))


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
  (reftex-default-bibliography '("~/hetzner/My-Library.bib"))
  (bibtex-completion-bibliography '( "~/hetzner/My-Library.bib"))
  (citar-bibliography '("~/hetzner/My-Library.bib"))
  (org-cite-global-bibliography '("~/hetzner/My-Library.bib"))
  (citar-file-note-org-include '(org-id org-roam-ref))
  (citar-notes-paths '("~/hetzner/roam-new/papers"))
  (citar-library-paths '("~/hetzner/Zotero/pdf"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-indicators (list citar-indicator-files citar-indicator-notes)))
  

(use-package citar-org-roam
  :ensure t
  :after citar org-roam
  :no-require
  :custom
  (citar-org-roam-capture-template-key "p")
  :config
  (citar-org-roam-mode))

(use-package org-ref
  :ensure t
  :bind (("C-c l" . org-ref-insert-link-hydra/body)))
  

(use-package org-roam-bibtex
  :ensure t
  :after org-roam
  :config
  (require 'org-ref)) ; optional: if using Org-ref v2 or v3 citation links


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
  (setq vertico-cycle t))
  

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
(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (python-mode . python-ts-mode)
        (java-mode . java-ts-mode)))
        
;; http://danmidwood.com/content/2014/11/21/animated-paredit.html
(use-package paredit
  :ensure t
  :hook ((clojure-mode . paredit-mode))
  )

(use-package paredit-menu
  :ensure t)

(use-package cider
  :ensure t)

;;First install the package:
;; (use-package flycheck-clj-kondo
;;   :ensure t)

;; (use-package clj-refactor
;;   :ensure t
;;   :config
;;   (defun my-clojure-mode-hook ()
;;     (clj-refactor-mode 1)
;;     (yas-minor-mode 1) ; for adding require/use/import statements
;;     ;; This choice of keybinding leaves cider-macroexpand-1 unbound
;;     (cljr-add-keybindings-with-prefix "C-c C-m"))
;;   (add-hook 'clojure-mode-hook #'my-clojure-mode-hook))



;;then install the checker as soon as `clojure-mode' is loaded
(use-package clojure-mode
  :ensure t
  :after (flycheck-clj-kondo clj-refactor)
  :config
  (require 'flycheck-clj-kondo)
  (flycheck-mode 1))

(use-package clj-refactor
  :ensure t)

(use-package neil
  :ensure t)

(dolist (hook '(prog-mode-hook))
  (add-hook hook (lambda ()
                   (display-line-numbers-mode 1))))
                   



(use-package corfu
  :ensure t
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

(use-package corfu-popupinfo
  :defer t
  :hook ((corfu-mode . corfu-popupinfo-mode))
  :config
  (setq corfu-popupinfo-delay '(0.5 . 1.0)))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :defer t
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Add extensions
(use-package cape
  :ensure t
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-TAB, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
  ;; Make these capfs composable.
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  ;;(advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive)  
)

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

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
;;(use-package git-gutter-fringe+
;;  :ensure t
;;  :hook
;; ((prog-mode . git-gutter+-mode)
;;   (org-mode . git-gutter+-mode)))
  

;; Use eglot for completions
(use-package eglot
 :ensure t)

;;(use-package eglot-java
;;  :ensure t)

;; (defun aw/cleanup-lsp ()
;;   "Remove all the workspace folders from LSP"
;;   (interactive)
;;   (let ((folders (lsp-session-folders (lsp-session))))
;;     (while folders
;;       (lsp-workspace-folders-remove (car folders))
;;       (setq folders (cdr folders)))))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :after lsp-mode
  :custom
  (lsp-ui-peek-always-show nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-doc-enable nil)
  
  )

;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
(use-package lsp-mode
  :ensure t
  :init
  :bind (:map lsp-mode-map
              ("C-M-."      . lsp-find-references)
              ;;("C-c r"      . lsp-rename)
              ("M-<return>" . lsp-execute-code-action))  
  :custom
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (lsp-keymap-prefix "C-c l")
  (lsp-inlay-hint-enable t)
  (lsp-lens-enable nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  ;; enable / disable the hints as you prefer:
  (lsp-inlay-hint-enable t)
  ;; These are optional configurations. See https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/#lsp-rust-analyzer-display-chaining-hints for a full list
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints nil)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints nil)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)  
  (lsp-signature-auto-activate t)
  
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (rustic-mode . lsp)
         ;; (go-mode . lsp)
         (java-ts-mode . lsp)
         (java-mode . lsp)
         (clojure-mode-hook . lsp)
         (clojurescript-mode-hook . lsp)
         (clojurec-mode-hook . lsp)         
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :commands lsp)

(use-package lsp-treemacs
  :ensure t)

;; (defun aw/cleanup-lsp ()
;;   "Remove all the workspace folders from LSP"
;;   (interactive)
;;   (let ((folders (lsp-session-folders (lsp-session))))
;;     (while folders
;;       (lsp-workspace-folders-remove (car folders))
;;       (setq folders (cdr folders)))))

;; (use-package lsp-java
;;   :ensure t
;;   :after lsp-mode
;;   :hook (
;;          (java-mode . #'lsp)
;;          ))

;; ;; optionally
;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode
;;   :custom
;;   (lsp-ui-doc-show-with-cursor t)
;;   )

;; ;; if you are helm user
;; ;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; ;; if you are ivy user
;; ;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; ;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; ;; optionally if you want to use debugger
;; (use-package dap-mode
;;   :ensure t
;;   )
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; Program in golang

;; (use-package tree-sitter
;;   :ensure t)

;; (use-package tree-sitter-langs
;;   :ensure t)
 
 
  
  

(use-package yaml-mode
 :ensure t)

;;(use-package go-mode
;;  :ensure t
;;  :bind (("C-c d" . flymake-show-buffer-diagnostics))
;;  :config (setq-local treesit-font-lock-feature-list
;;                      '(( comment definition)
;;                        ( keyword string type)
;;                        ( assignment builtin constant decorator
;;                          escape-sequence number property string-interpolation)
;;                        ( bracket delimiter function operator variable)))
;;  :hook (go-mode . eglot-ensure))

;; Enable formatting on save automatically
(use-package format-all
 :ensure t
 :commands format-all-mode
 :hook (prog-mode . format-all-mode)
 :config
 (setq-default format-all-formatters '(("Go"     (goimports))
                                       ("Java"   (astyle))
                                       ("html"   (prettierd))
                                       ("rust"   (rustfmt)))))
  
;; (defun setup-rust ()
;;   "Setup for ‘rust-mode’."
;;   ;; Configuration taken from rust-analyzer’s manual:
;;   ;; https://rust-analyzer.github.io/manual.html#configuration
;;   (setq-local eglot-workspace-configuration
;;               ;; Setting the workspace configuration for every
;;               ;; rust-mode buffer, you can also set it with dir-local
;;               ;; variables, should you want different configuration
;;               ;; per project/directory.
;;               '(:rust-analyzer
;;                 ( :procMacro ( :attributes (:enable t)
;;                                :enable t)
;;                   :cargo (:buildScripts (:enable t))
;;                   :diagnostics (:disabled ["unresolved-proc-macro"
;;                                            "unresolved-macro-call"])))))


;; ;; Define a custom eglot LSP server for rust-analyzer because it
;; ;; expects initializationOptions done a bit differently (see below).
;; (defclass eglot-rust-analyzer (eglot-lsp-server) ()
;;  :documentation "A custom class for rust-analyzer.")

;; ;; Rust-analyzer requires the workspaceConfiguration sent as
;; ;; initializationOptions at startup time. See
;; ;; https://github.com/joaotavora/eglot/discussions/845 and
;; ;; rust-analyzer’s manual page.
;; (cl-defmethod eglot-initialization-options ((server eglot-rust-analyzer))
;;  eglot-workspace-configuration)

;; ;; Use our custom ‘eglot-rust-analyzer’ for ‘rust-mode’.
;; ;;(add-to-list 'eglot-server-programs
;; ;;             '(rust-mode . (eglot-rust-analyzer "rust-analyzer")))
 ;; Run our setup function in ‘rust-mode-hook’.
;; (add-hook 'rust-mode-hook #'setup-rust)

;;
;; The Rust Programming language
;;

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))


(use-package nix-mode
  :ensure t)

;; (use-package rustic
;;   :ensure t)

;; (use-package terraform-mode
;;   :ensure t)

;; (use-package dockerfile-mode
;;   :ensure t)

(use-package web-mode
  :ensure t)

(use-package emmet-mode
  :ensure t)

;; -- BLOGGING

(use-package ox-hugo
  :ensure t)

(use-package markdown-mode
  :ensure t)

;; -- Latex

(use-package reftex
  :ensure t
  :hook (LaTeX-mode . reftex-mode)
  :custom
  ;; Get RefTeX working with BibLaTeX, see
  ;; http://tex.stackexchange.com/questions/31966/setting-up-reftex-with-biblatex-citation-commands/31992#31992.
  (setq reftex-cite-format
        '((?a . "\\autocite[]{%l}")
          (?b . "\\blockcquote[]{%l}{}")
          (?c . "\\cite[]{%l}")
          (?f . "\\footcite[]{%l}")
          (?n . "\\nocite{%l}")
          (?p . "\\parencite[]{%l}")
          (?s . "\\smartcite[]{%l}")
          (?t . "\\textcite[]{%l}"))
        (region-end)ftex-plug-into-AUCTeX t
        reftex-toc-split-windows-fraction 0.3
        ;; This is needed when `reftex-cite-format' is set. See
        ;; https://superuser.com/a/1386206
        LaTeX-reftex-cite-format-auto-activate nil))
  

;; (use-package tex
;;   :ensure auctex)

;; (use-package auctex
;;   :ensure t
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
;; 
;;  (setq TeX-parse-self t ; parse on load
;;         TeX-auto-save t  ; parse on save
;;         ;; Use hidden directories for AUCTeX files.
;;         TeX-auto-local ".auctex-auto"
;;         TeX-style-local ".auctex-style"
;;         TeX-source-correlate-mode t
;;         TeX-source-correlate-method 'synctex
;;         ;; Don't start the  server when correlating sources.
;;         TeX-source-correlate-start-server nil
;;         ;; Automatically insert braces after sub/superscript in `LaTeX-math-mode'.
;;        TeX-electric-sub-and-superscript t
;;         ;; Just save, don't ask before each compilation.
;;         TeX-save-query nil
;;         LaTeX-section-hook '(LaTeX-section-heading
;;                              LaTeX-section-title
;;                              LaTeX-section-toc
;;                              LaTeX-section-section
;;                              LaTeX-section-label)
;;          LaTeX-fill-break-at-separators nil
;;          LaTeX-item-indent 0))

(use-package ledger-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)


;; -- Nursery projects
;; git clone git@github.com:chrisbarrett/nursery.git nursery
(add-to-list 'load-path "~/.config/emacs/nursery/lisp")
(use-package org-roam-dblocks
  :hook (org-mode . org-roam-dblocks-autoupdate-mode))


(use-package org-journal
  :ensure t
  :defer t
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j ")
  :config
  (setq org-journal-dir "~/hetzner/roam-new/journal/"
        org-journal-file-format "%Y.org"
        org-journal-file-type 'yearly
        org-journal-date-format "%A, %d %B %Y"))

(use-package gptel
  :ensure t
  :bind (("C-c RET" . gptel-send)))

(set-face-font 'default "JetbrainsMono Nerd Font-12")

(add-hook 'after-init-hook #'global-flycheck-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fbf73690320aa26f8daffdd1210ef234ed1b0c59f3d001f342b9c0bbf49f531c"
     default))
 '(package-selected-packages
   '(all-the-icons cape catppuccin-theme chatgpt-shell citar-org-roam
                   clj-refactor clojure-lsp corfu dockerfile-mode
                   doom-modeline emmet-mode envrc flycheck-clj-kondo
                   format-all gptel imenu-list ledger-mode lsp-mode
                   lsp-treemacs lsp-ui lsp-ui-mode magit marginalia
                   modus-themes neil nerd-icons-completion
                   nerd-icons-corfu nix-mode olivetti orderless
                   org-download org-journal org-modern org-noter
                   org-ref org-roam-bibtex org-roam-ui
                   org-super-agenda ox-hugo paredit-menu pdf-tools
                   projectile-ripgrep rustic smartparens
                   spacious-padding vertico vterm-toggle vulpea
                   web-mode yaml-mode))
 '(safe-local-variable-values
   '((lsp-ltex-language . "nl") (lsp-ltex-language . nl-NL)
     (ispell-dictionary . "nl") (lsp-ltex-language . "nl-NL")
     (ispell-dictionary . "nl_NL"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t :background "#ffffff")))
 '(header-line ((t :box (:line-width 4 :color "#f2f2f2" :style nil))))
 '(header-line-highlight ((t :box (:color "#000000"))))
 '(keycast-key ((t)))
 '(line-number ((t :background "#ffffff")))
 '(mode-line ((t :box (:line-width 6 :color "#ccdfff" :style nil))))
 '(mode-line-active ((t :box (:line-width 6 :color "#ccdfff" :style nil))))
 '(mode-line-highlight ((t :box (:color "#000000"))))
 '(mode-line-inactive ((t :box (:line-width 6 :color "#e6e6e6" :style nil))))
 '(tab-bar-tab ((t :box (:line-width 4 :color "#ffffff" :style nil))))
 '(tab-bar-tab-inactive ((t :box (:line-width 4 :color "#c2c2c2" :style nil))))
 '(tab-line-tab ((t)))
 '(tab-line-tab-active ((t)))
 '(tab-line-tab-inactive ((t)))
 '(vertical-border ((t :background "#ffffff" :foreground "#ffffff")))
 '(window-divider ((t (:background "#ffffff" :foreground "#ffffff"))))
 '(window-divider-first-pixel ((t (:background "#ffffff" :foreground "#ffffff"))))
 '(window-divider-last-pixel ((t (:background "#ffffff" :foreground "#ffffff")))))
