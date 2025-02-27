;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
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

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/hetzner/roam-new/")
(setq org-roam-directory "~/hetzner/roam-new/")

(after! org

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

;; Clojure coding
(use-package! paredit
  :hook (clojure-mode . enable-paredit-mode))

(use-package! vulpea
  :config
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
    ;; (append
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
        :from tags
        :left-join nodes
        :on (= tags:node-id nodes:id)
        :where (like tag (quote "%\"planner\"%"))])))
    ;;(aw/gather-agenda-files))
    ;; depends on the function from org definition above
    )

  (defun vulpea-agenda-files-update (&rest _)
    "Update the value of `org-agenda-files'."
    (setq org-agenda-files (vulpea-project-files)))
  )


(use-package! org
  :after vulpea
  :custom
  ;; populate org-agenda-files using vulpea, which queries org-roam
  (setq org-agenda-files #'(vulpea-project-files)  )
  (add-hook 'before-save-hook #'vulpea-project-update-tag)
  (advice-add 'org-agenda :before #'vulpea-agenda-files-update)
  (advice-add 'org-todo-list :before #'vulpea-agenda-files-update)
  )

;; -- Nursery projects
;; git clone git@github.com:chrisbarrett/nursery.git nursery
(use-package! org-roam-dblocks
  :hook (org-mode . org-roam-dblocks-autoupdate-mode))

(use-package! lsp-java
  :config
  (require 'lsp-java-boot)

  :hook
  ((lsp-mode-hook . #'lsp-lens-mode)
   (java-mode-hook . #'lsp-java-boot-lens-mode)))

(use-package! denote
  :config
  (require 'denote-journal-extras)
  (setq denote-directory (expand-file-name "~/hetzner/denote/"))
  (setq denote-journal-extras-title-format 'day-date-month-year)
  (setq denote-journal-extras-directory (expand-file-name "~/hetzner/denote/20-29 Life Admin/21 Mental Health/21.01 Daily Journal"))
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories))
