;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;;(doom/set-frame-opacity 97)
(setq fancy-splash-image (expand-file-name "assets/blackhole-lines.svg" doom-user-dir))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Arjen Wiersma"
      user-mail-address "arjen@wiersma.org")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; When you want to profile your startup time
;; (use-package! benchmark-init
;;   :ensure t
;;   :config
;;   (add-hook 'doom-first-input-hook #'benchmark-init/deactivate))


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'modus-vivendi)

(setq modus-themes-to-toggle '(modus-operandi modus-vivendi))

;; (use-package! spacious-padding
;;   :config
;;   (spacious-padding-mode))

(setq
 default-font "JetBrainsMono Nerd Font"
 default-font-size 16.0
 default-nice-size 12.0
 doom-font-increment 1
 doom-font (font-spec :family default-font
                      :size default-font-size)
 doom-symbol-font (font-spec :family default-font
                             :size default-font-size)
 doom-variable-pitch-font (font-spec :family "iMWritingDuoNerdFont"
                                     :size default-font-size)
 doom-modeline-buffer-file-name-style 'truncate-with-project)

(setq-default tab-width 2)

(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)

(custom-set-variables '(emojify-display-style 'unicode))
(setq exec-path (append exec-path '("/home/arjen/.local/bin")))
;; doom-unicode-font (font-spec :family "IBM Plex Mono"
;;                              :size default-font-size)
;; doom-serif-font (font-spec :family "IBM Plex Serif"
;;                            :size default-nice-size)


;; (defvar *arjen-theme-dark* 'catppuccin)
;; (defvar *arjen-theme-light* 'modus-operandi)
;; (defvar *arjen-current-theme* *arjen-theme-dark*)

;; ;; disable other themes before loading new one
;; (defadvice load-theme (before theme-dont-propagate activate)
;;   "Disable theme before loading new one."
;;   (mapc #'disable-theme custom-enabled-themes))


;; (defun arjen/next-theme (theme)
;;   (if (eq theme 'default)
;;       (disable-theme *arjen-current-theme*)
;;     (progn
;;       (load-theme theme t)))
;;   (setq *arjen-current-theme* theme))

;; (defun arjen/toggle-theme ()
;;   (interactive)
;;   (cond ((eq *arjen-current-theme* *arjen-theme-dark*) (arjen/next-theme *arjen-theme-light*))
;;         ((eq *arjen-current-theme* *arjen-theme-light*) (arjen/next-theme *arjen-theme-dark*))))
;;         ;;((eq *arjen-current-theme* 'default) (arjen/next-theme *arjen-theme-dark*))

(defvar *is-light* nil)
(defun arjen/toggle-theme()
  (interactive)
  (if *is-light*
      (progn
        (setq *is-light* nil)
        (setq catppuccin-flavor 'mocha) ;; or 'latte, 'macchiato, or 'mocha
        (catppuccin-reload))

    (progn
      (setq *is-light* t)
      (setq catppuccin-flavor 'latte) ;; or 'latte, 'macchiato, or 'mocha
      (catppuccin-reload))))

(global-set-key [f5] 'modus-themes-toggle)



;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/hetzner/roam-new/")
(setq org-roam-directory "~/hetzner/roam-new/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; The default `jk' escape sequence interferes with the dutch language.
(after! evil-escape
  (setq evil-escape-key-sequence "qp"))

;; (after! lsp-ui
;;   (setq lsp-ui-sideline-enable nil)
;;   (setq lsp-ui-sideline-show-code-actions nil)
;;   (setq lsp-ui-sideline-enable nil))

(use-package! vulpea)

(after! vulpea
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
          ""))))

(use-package! org-roam
  :config
  (require 'org-roam-protocol)
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
          ("l"
           "learntostudy" plain "%?"
           :if-new
           (file+head "learntostudy/%<%Y%m%d%H%M%S>-${slug}.org"
                      "#+title: ${title}\n#+filetags: :learntostudy:\n")
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
          ))
  (require 'org-roam-protocol)
  (defun my/preview-fetcher ()
    (let* ((elem (org-element-context))
           (parent (org-element-property :parent elem)))
      ;; TODO: alt handling for non-paragraph elements
      (string-trim-right (buffer-substring-no-properties
                          (org-element-property :begin parent)
                          (org-element-property :end parent)))))

  (setq org-roam-preview-function #'my/preview-fetcher)
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* TODO %?"
           :target (file+head "%<%Y>/%<%Y-%m-%d>.org" "#+TITLE: %<%B %d, %Y>
#+filetags: dailies

- tags :: [[id:6b2b4539-b6c0-4966-ae41-ff9048be1e86][Daily Notes]]
* 📅 Dagelijkse vragen

** 🎯 Vandaag wil ik bereiken...

** 👏 Iets waar ik naar uit kijk...

** 👎 Hier worstel ik momenteel mee...

* 🌱 Daily Habits

- [ ] Plan de dag
- [ ] Lees een boek
- [ ] Reflecteer op de dag
- [ ] De dag afsluiten, geen open taken

* Captured items

* Meta
# Local Variables:
# ispell-dictionary: \"nl_NL\"
# End:
")))))

(after! org

  (setq!
   org-agenda-files #'(vulpea-project-files)

   org-attach-directory "~/hetzner/roam-new/.attach/"

   org-refile-targets '(("/home/arjen/hetzner/roam-new/20231008105247-planning.org" :maxlevel . 4)
                        ("/home/arjen/hetzner/roam-new/20231008105710-tickler.org" :maxlevel . 2)))

  (setq org-id-link-to-org-use-id t)
  (setq org-image-actual-width 400)
  (setq org-log-into-drawer t)

  ;; (add-load-path! (concat doom-user-dir "org-protocol-capture-html"))
  ;; (require 'org-protocol-capture-html)
  (setq! org-capture-templates '(("b" "Blog idea" entry (file+olp "~/hetzner/roam-new/20231008105247-planning.org" "Inbox" "Series")
                                  "* %?\n%T" :prepend t)
                                 ("t" "todo" entry
                                  (file+headline "~/hetzner/roam-new/20231008105247-planning.org" "Inbox")
                                  "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
                                 ("T" "Tickler" entry
                                  (file+headline "~/hetzner/roam-new/20231008105247-planning.org" "Inbox")
                                  "* %i%? \n %U")
                                 ("w" "Web site" entry
                                  (file "")
                                  "* %a :website:\n\n%U %?\n\n%:initial")
                                 ("wN" "Web link" entry
                                  (file+headline ,(car org-agenda-files)
                                                 "Links to read later")
                                  "* TODO [#A]  %?%a \nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"Fri\"))\n"
                                  :immediate-finish t :empty-lines 1)
                                 ("e" "email" entry (file+headline "~/hetzner/roam-new/20231008105247-planning.org" "Inbox")
                                  "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n"))))


(after! citar
  (setq! reftex-default-bibliography "/home/arjen/hetzner/My-Library.bib"
         bibtex-completion-bibliography '("/home/arjen/hetzner/My-Library.bib")
         citar-file-note-org-include '(org-id org-roam-ref)
         citar-bibliography '("~/hetzner/My-Library.bib")
         citar-notes-paths '("~/hetzner/roam/papers")
         citar-library-paths '("~/hetzner/Zotero/pdf"))


  (map! :map latex-mode-map
        :localleader
        "n i" #'citar-insert-citation)
  (map! :map org-mode-map
        :localleader
        "n i" #'citar-insert-citation))

(after! cider
  (setq cljr-warn-on-eval nil))


;; Add word count to mode-line, only useful for modes like org and markdown
;; for writing papers and articles
;; (setq doom-modeline-enable-word-count t)
(setq company-idle-delay nil)

(defun aw/cleanup-lsp ()
  "Remove all the workspace folders from LSP"
  (interactive)
  (let ((folders (lsp-session-folders (lsp-session))))
    (while folders
      (lsp-workspace-folders-remove (car folders))
      (setq folders (cdr folders)))))

(defun tl/dired-copy-path-at-point ()
  (interactive)
  (dired-copy-filename-as-kill 0))

(after! dired
  (define-key dired-mode-map (kbd "W") 'tl/dired-copy-path-at-point))


(defun move-file (new-location)
  "Write this file to NEW-LOCATION, and delete the old one."
  (interactive (list (expand-file-name
                      (if buffer-file-name
                          (read-file-name "Move file to: ")
                        (read-file-name "Move file to: "
                                        default-directory
                                        (expand-file-name (file-name-nondirectory (buffer-name))
                                                          default-directory))))))
  (when (file-exists-p new-location)
    (delete-file new-location))
  (let ((old-location (expand-file-name (buffer-file-name))))
    (write-file new-location t)
    (when (and old-location
               (file-exists-p new-location)
               (not (string-equal old-location new-location)))
      (delete-file old-location))))

(use-package! citar-org-roam
  :defer t
  :config
  (setq citar-org-roam-capture-template-key "p")
  ;;(setq citar-org-roam-note-title-template "${author} - ${title}\n#+filetags: ${tags}")
  (citar-org-roam-mode))

;;(require 'all-the-icons)
(use-package! all-the-icons
  :config
  (customize-set-value
   'org-agenda-category-icon-alist
   `(
     ("inbox" ,(list (all-the-icons-faicon "inbox")) nil nil :ascent center)
     ("gcal-novi" ,(list (all-the-icons-faicon "building-o")) nil nil :ascent center)
     ("gcal-gezin" ,(list (all-the-icons-faicon "users")) nil nil :ascent center)
     ("gcal-ou" ,(list (all-the-icons-faicon "university")) nil nil :ascent center)
     ("daily" ,(list (all-the-icons-faicon "circle-o-notch")) nil nil :ascent center)
     ("work" ,(list (all-the-icons-faicon "cogs")) nil nil :ascent center)
     ("habit" ,(list (all-the-icons-faicon "circle-o-notch")) nil nil :ascent center)
     ("study" ,(list (all-the-icons-faicon "university")) nil nil :ascent center)
     ("notes" ,(list (all-the-icons-faicon "clipboard")) nil nil :ascent center))))

(use-package! org-super-agenda
  :after org-agenda
  :init
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
                                         ("tickler", (list (all-the-icons-faicon "clock-o" :heigh 0.8)) nil nil :ascent center))
        )
  ;; (setq org-super-agenda-groups '((:name "Today"
  ;;                                  :time-grid t
  ;;                                  :scheduled today)
  ;;                                 (:name "Due Today"
  ;;                                  :scheduled today)
  ;;                                 (:name "Important"
  ;;                                  :priority "A")
  ;;                                 (:name "Overdue"
  ;;                                  :deadline past)
  ;;                                 (:name "Overdue scheduled"
  ;;                                  :scheduled past)
  ;;                                 (:name "Due soon"
  ;;                                  :deadline future)
  ;;                                 (:priority<= "B"
  ;;                                  :order 1)))
  ;;  (org-super-agenda-mode t)
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
           :face 'warning))
	)
  
  ;; (setq  org-deadline-warning-days 7
  ;;        org-agenda-breadcrumbs-separator " ❱ ")

  :config
  (org-super-agenda-mode))

(after! org
  (use-package! org-drill)
  ;; https://www.reddit.com/r/emacs/comments/hnf3cw/my_orgmode_agenda_much_better_now_with_category/
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
                                    (org-agenda-todo-keyword-format ""))))))))

(use-package! org-remark
  :config
  (org-remark-global-tracking-mode t))

(use-package! org-roam-dblocks
  :defer t)

(after! org
  (defun my/org-tags-view (&optional todo-only watch)
    (let ((org-use-tag-inheritance nil))
      (org-tags-view todo-only watch)))

  (advice-add 'org-tags-view :around
              (lambda (orig-fun &rest args)
                (let ((org-use-tag-inheritance nil))
                  (apply orig-fun args)))))

(use-package! ox-hugo)

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

(use-package! company-org-block
  :custom
  (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
  :hook ((org-mode . (lambda ()
                       (setq-local company-backends '(company-org-block))
                       (company-mode +1)))))

(use-package! org-ref)

(after! org
  (setq org-modern-block-fringe 2)
  ;; (setq org-modern-table nil)
  ;; (setq org-modern-timestamp nil)
  ;;(global-org-modern-mode)
  ;; Ignore optie voor header exports
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))

  ;; Toevoeging van LaTeX book class zonder parts
  (add-to-list 'org-latex-classes
               '("chapterbook"
                 "\\documentclass{book}"
                 ("\\chapter{%s}" . "\\chapter{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")))

  (setq org-latex-src-block-backend 'listings))

(setq ispell-program-name "hunspell")

;; PROGRAMMING

(after! projectile
  (setq projectile-create-missing-test-files t))

;; Instead of lsp-mode, eglot is also available. It is somewhat harder to configure for non-supported
;; modes, and it does not allow multi-mode files (web + tailwind)
(use-package! eglot-java
  :hook ((java-mode . eglot-java-mode)))

(use-package! eglot
  ;; :hook ((( clojure-mode clojurec-mode clojurescript-mode java-mode go-mode . eglot-ensure)
  ;;         ((cider-mode eglot-managed-mode) . eglot-disable-in-cider)))
  ;; :preface
  ;; (defun eglot-disable-in-cider ()
  ;;   (when (eglot-managed-p)
  ;;     (if (bound-and-true-p cider-mode)
  ;;         (progn
  ;;           (remove-hook 'completion-at-point-functions 'eglot-completion-at-point t)
  ;;           (remove-hook 'xref-backend-functions 'eglot-xref-backend t))
  ;;       (add-hook 'completion-at-point-functions 'eglot-completion-at-point nil t)
  ;;       (add-hook 'xref-backend-functions 'eglot-xref-backend nil t))))
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-extend-to-xref nil)
  (eglot-ignored-server-capabilities
   '(:hoverProvider
     :documentHighlightProvider
     :documentFormattingProvider
     :documentRangeFormattingProvider
     :documentOnTypeFormattingProvider
     :colorProvider
     :foldingRangeProvider))
  (eglot-stay-out-of '(yasnippet)))

;; (after! go-mode
;;   (add-hook 'go-mode-hook 'eglot-ensure)
;;   (defun +eglot-organize-imports() (call-interactively 'eglot-code-action-organize-imports))
;;   (add-hook 'before-save-hook '+eglot-organize-imports nil t))

;; (add-to-list 'eglot-server-programs
;;              '((web-mode :language-id "html") . ("npx" "tailwindcss-language-server" "--stdio")))

;; (use-package! web-mode
;;   :config
;;   (add-to-list 'eglot-server-programs
;;                '((web-mode :language-id "html") . ("vscode-html-language-server" "--stdio"))))

;; fix until resolved: https://github.com/doomemacs/doomemacs/issues/7733
(defun my/org-tab-conditional ()
  (interactive)
  (if (yas-active-snippets)
      (yas-next-field-or-maybe-expand)
    (org-cycle)))

(map! :after evil-org
      :map evil-org-mode-map
      :i "<tab>" #'my/org-tab-conditional)


;; -- denote experiment
(use-package! denote
  :custom
  (denote-directory (expand-file-name "~/hetzner/denote"))
  (require 'denote-journal-extras)
  :config
  (add-hook 'dired-mode-hook #'denote-dired-mode)
  (map! :leader
        (:prefix-map ("d" . "denote")
         :desc "Show backlinks" "b" #'denote-backlinks
         :desc "Create a note" "d" #'denote
         :desc "Find a note" "f" #'consult-denote-find
         :desc "Insert a link" "i" #'denote-link
         :desc "List all links" "l" #'denote-find-link
         :desc "Create a note in a subdirectory" "s" #'denote-subdirectory
         :desc "Create a note based on a template" "t" #'denote-template
         :desc "Rename file" "r" #'denote-rename-file-using-front-matter
         ))

  (with-eval-after-load 'org-capture
    (setq denote-org-capture-specifiers "%l\n%i\n%?")
    (add-to-list 'org-capture-templates
                 '("n" "New note (with denote.el)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t)))

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
  )

(use-package! denote-journal-extras
  :ensure nil
  :after denote)

(use-package! denote-explore
  :after denote
  :custom
  ;; Where to store network data and in which format
  (denote-explore-network-directory "<folder>")
  (denote-explore-network-filename "<filename?")
  (denote-explore-network-format 'graphviz)
  :bind
  (;; Statistics
   ("C-c w e c" . denote-explore-count-notes)
   ("C-c w e C" . denote-explore-count-keywords)
   ("C-c w e b" . denote-explore-keywords-barchart)
   ("C-c w e x" . denote-explore-extensions-barchart)
   ;; Random walks
   ("C-c w e r" . denote-explore-random-note)
   ("C-c w e l" . denote-explore-random-link)
   ("C-c w e k" . denote-explore-random-keyword)
   ;; Denote Janitor
   ("C-c w e d" . denote-explore-identify-duplicate-notes)
   ("C-c w e z" . denote-explore-zero-keywords)
   ("C-c w e s" . denote-explore-single-keywords)
   ("C-c w e o" . denote-explore-sort-keywords)
   ("C-c w e r" . denote-explore-rename-keywords)
   ;; Visualise denote
   ("C-c w e n" . denote-explore-network)
   ("C-c w e v" . denote-explore-network-regenerate)
   ("C-c w e D" . denote-explore-degree-barchart)))

(use-package! consult)

(use-package! consult-denote
  :bind
  (("C-c C-d" . consult-denote-find)))

;; (use-package denote-citar
;;   :ensure t
;;   :recipe (:type git :host github :repo "pprevos/denote-citar"))

(use-package! org-re-reveal)

(use-package! org-excalidraw
  :commands (org-excalidraw-create-drawing)
  :config
  (setq org-excalidraw-default-directory "~/hetzner/roam-new/.attach/excalidraw/"))

;; (require 'org-excalidraw)
(after! corfu
  (setq corfu-auto nil)
  (setq corfu-auto-delay 1))
