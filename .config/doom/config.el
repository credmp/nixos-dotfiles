;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

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
(setq doom-theme 'catppuccin)

(setq
 default-font "JetBrainsMono Nerd Font"
 default-font-size 16.0
 default-nice-size 12.0
 doom-font-increment 1
 doom-font (font-spec :family default-font
                      :size default-font-size)
 doom-unicode-font (font-spec :family default-font
                              :size default-font-size))
;; doom-variable-pitch-font (font-spec :family "IBM Plex Sans Condensed"
;;                                     :size default-font-size)
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
        (setq catppuccin-flavor 'latte) ;; or 'latte, 'macchiato, or 'mocha
        (catppuccin-reload))

    (progn
      (setq *is-light* t)
      (setq catppuccin-flavor 'mocha) ;; or 'latte, 'macchiato, or 'mocha
      (catppuccin-reload))))


(global-set-key [f5] 'arjen/toggle-theme)



;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/stack/roam-new/")
(setq org-roam-directory "~/stack/roam-new/")


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

(after! lsp-ui
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-ui-sideline-enable nil))



;;(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(add-to-list 'load-path "/home/arjen/.nix-profile/share/emacs/site-lisp/mu4e")
(after! mu4e
  ;; Mail configuration
  ;; add to $DOOMDIR/config.el

  (setq sendmail-program (executable-find "msmtp")
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        mu4e-context-policy 'ask
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail)

  (set-email-account! "Fastmail"
                      '((mu4e-sent-folder       . "/fm/Sent Items")
                        (mu4e-drafts-folder     . "/fm/Drafts")
                        (mu4e-trash-folder      . "/fm/Trash")
                        (mu4e-refile-folder     . "/fm/Archive")
                        (smtpmail-smtp-user     . "arjen@wiersma.org")
                        (user-mail-address      . "arjen@wiersma.org")    ;; only needed for mu < 1.4
                        (mu4e-compose-signature . "---\nYours truly\nThe Baz"))
                      t)

  (set-email-account! "GMail"
                      '((mu4e-sent-folder       . "/fm/Sent Items")
                        (mu4e-drafts-folder     . "/fm/Drafts")
                        (mu4e-trash-folder      . "/fm/Trash")
                        (mu4e-refile-folder     . "/fm/Archive")
                        (smtpmail-smtp-user     . "arjenw@gmail.com")
                        (user-mail-address      . "arjenw@gmail.com")    ;; only needed for mu < 1.4
                        (mu4e-compose-signature . "---\nYours truly\nThe Baz"))
                      t))


(use-package! org-roam
  :config
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

** 🌙 Gisteravond heb ik...

** 🚀 Vandaag wil ik bereiken...

** 👏 Iets waar ik naar uit kijk...

** 👎 Hier worstel ik momenteel mee...

* Routine

- [ ] Plan de dag
- [ ] Werken aan mijn thesis
- [ ] De dag afsluiten, geen open taken

* Captured items

* Meta
# Local Variables:
# ispell-dictionary: \"nl_NL\"
# End:
")))))



;; (use-package! org-roam-review
;;   :commands (org-roam-review
;;              org-roam-review-list-by-maturity
;;              org-roam-review-list-recently-added)
;;   ;; Optional - tag all newly-created notes as seedlings
;;   :hook (org-roam-capture-new-node . org-roam-review-set-seedling)
;;   :general
;;   ;; Optional - bindings for evil-mode compatability.
;;   (:states '(normal) :keymaps 'org-roam-review-mode-map
;;    "TAB" 'magit-section-cycle
;;    "g r" 'org-roam-review-refresh)
;;   (:keymaps 'org-mode-map
;;    "C-c r r" '(org-roam-review-accept :wk "accept")
;;    "C-c r u" '(org-roam-review-bury :wk "bury")
;;    "C-c r x" '(org-roam-review-set-excluded :wk "set excluded")
;;    "C-c r b" '(org-roam-review-set-budding :wk "set budding")
;;    "C-c r s" '(org-roam-review-set-seedling :wk "set seedling")
;;    "C-c r e" '(org-roam-review-set-evergreen :wk "set evergreen")))

;; Ensure that you have languagetool 5.8 extracted in .doom.d
;; wget https://languagetool.org/download/LanguageTool-5.8.zip
;; (use-package! langtool)

(after! langtool
  (setq langtool-language-tool-server-jar (concat doom-user-dir "/LanguageTool-5.8/languagetool-server.jar")))
;; :bind (("\C-x4w" . langtool-check)
;;        ("\C-x4W" . langtool-check-done)
;;        ("\C-x4l" . langtool-switch-default-language)
;;        ("\C-x44" . langtool-show-message-at-point)
;;        ("\C-x4c" . langtool-correct-buffer))

;; (require 'find-lisp)


(after! org
  ;; (map! :map org-mode-map
  ;;       :localleader
  ;;       "L c" #'langtool-check
  ;;       "L d" #'langtool-check-done
  ;;       "L s" #'langtool-switch-default-language
  ;;       "L m" #'langtool-show-message-at-point
  ;;       "L x" #'langtool-correct-buffer)


  (setq! org-agenda-files ;; (append
         ;;   (find-lisp-find-files "/home/arjen/stack/Notebook/" ".org$"))
         ;;(find-lisp-find-files "/home/arjen/stack/roam-new/" ".org$")


         '("/home/arjen/stack/roam-new/20231008105247-planning.org"
           "/home/arjen/stack/roam-new/📥 Inbox.org"
           "/home/arjen/stack/roam-new/20231008105710-tickler.org")


         org-refile-targets '(("/home/arjen/stack/roam-new/20231008105247-planning.org" :maxlevel . 4)
                              ("/home/arjen/stack/roam-new/20231008105710-tickler.org" :maxlevel . 2)))


  (setq org-id-link-to-org-use-id t)
  (setq org-image-actual-width 800)
  (setq org-log-into-drawer t)

  ;; (add-load-path! (concat doom-user-dir "org-protocol-capture-html"))
  ;; (require 'org-protocol-capture-html)
  (setq! org-capture-templates '(("b" "Blog idea" entry (file+olp "~/stack/Notebook/notes.org" "Personal" "Series")
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
  ;; export code listings for latex
  (setq org-latex-listings t))

(after! citar
  (setq! reftex-default-bibliography "/home/arjen/stack/Studie/Open-Universiteit/My-Library.bib"
         bibtex-completion-bibliography '("/home/arjen/stack/Studie/Open-Universiteit/My-Library.bib")
         citar-file-note-org-include '(org-id org-roam-ref)
         citar-bibliography '("~/stack/Studie/Open-Universiteit/My-Library.bib")
         citar-notes-paths '("~/stack/roam/papers")
         citar-library-paths '("~/stack/Zotero/pdf"))


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
(setq doom-modeline-enable-word-count t)

;; Setup rust

(add-hook! rustic-mode-hook #'lsp-rust-analyzer-inlay-hints-mode)

(after! rustic
  (setq! lsp-rust-analyzer-server-display-inlay-hints t))

(map! :map lsp-mode-map
      :localleader
      "i" #'lsp-ui-imenu
      "l" #'lsp-avy-lens
      "d g" #'lsp-ui-doc-glance
      "d f" #'lsp-ui-doc-focus-frame
      "d u" #'lsp-ui-doc-unfocus-frame)

(setq company-idle-delay nil)


(defun aw/cleanup-lsp ()
  "Remove all the workspace folders from LSP"
  (interactive)
  (let ((folders (lsp-session-folders (lsp-session))))
    (while folders
      (lsp-workspace-folders-remove (car folders))
      (setq folders (cdr folders)))))


;; (lsp-session-folders (lsp-session))
;; (setq rmh-elfeed-org-files '("/home/arjen/stack/Notebook/elfeed.org"))

;; Rust Debugging
(setq dap-cpptools-extension-version "1.12.4")

;; (with-eval-after-load 'lsp-rust
;;         (require 'dap-cpptools))

;; (with-eval-after-load 'dap-cpptools
;;         ;; Add a template specific for debugging Rust programs.
;;         ;; It is used for new projects, where I can M-x dap-edit-debug-template
;;         (dap-register-debug-template "Rust::CppTools Run Configuration"
;;                                            (list :type "cppdbg"
;;                                                  :request "launch"
;;                                                  :name "Rust::Run"
;;                                                  :MIMode "gdb"
;;                                                  :miDebuggerPath "rust-gdb"
;;                                                  :environment []
;;                                                  :program "${workspaceFolder}/target/debug/hello / replace with binary"
;;                                                  :cwd "${workspaceFolder}"
;;                                                  :console "external"
;;                                                  :dap-compilation "cargo build"
;;                                                  :dap-compilation-dir "${workspaceFolder}")))

;; (with-eval-after-load 'dap-mode
;;         (setq dap-default-terminal-kind "integrated") ;; Make sure that terminal programs open a term for I/O in an Emacs buffer
;;         (dap-auto-configure-mode +1))

;; JAVA - add lombok

(setq path-to-lombok "/home/arjen/.m2/repository/org/projectlombok/lombok/1.18.28/lombok-1.18.28.jar")

(setq lsp-java-vmargs
      `("-noverify"
        "-Xmx1G"
        "-XX:+UseG1GC"
        "-XX:+UseStringDeduplication"
        ,(concat "-javaagent:" path-to-lombok)))
;;,(concat "-Xbootclasspath/a:" path-to-lombok)

;; Copy full path of item in direct

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

(use-package! org-transclusion
  :after org
  :init
  (map!
   :map global-map "<f12>" #'org-transclusion-add
   :leader
   :prefix "n"
   :desc "Org Transclusion Mode" "t" #'org-transclusion-mode))


;; (use-package! denote
;;   :config
;;   ;; Remember to check the doc strings of those variables.
;;   (setq denote-directory (expand-file-name "~/stack/denote/"))
;;   (setq denote-known-keywords '("emacs"))
;;   (setq denote-infer-keywords t)
;;   (setq denote-sort-keywords t)
;;   (setq denote-file-type nil) ; Org is the default, set others here
;;   (setq denote-prompts '(title keywords))
;;   (setq denote-excluded-directories-regexp nil)

;;   ;; Pick dates, where relevant, with Org's advanced interface:
;;   (setq denote-date-prompt-use-org-read-date t)


;;   ;; Read this manual for how to specify `denote-templates'.  We do not
;;   ;; include an example here to avoid potential confusion.


;;   ;; We allow multi-word keywords by default.  The author's personal
;;   ;; preference is for single-word keywords for a more rigid workflow.
;;   (setq denote-allow-multi-word-keywords t)

;;   (setq denote-date-format nil) ; read doc string

;;   ;; By default, we do not show the context of links.  We just display
;;   ;; file names.  This provides a more informative view.
;;   (setq denote-backlinks-show-context t)

;;   ;; Also see `denote-link-backlinks-display-buffer-action' which is a bit
;;   ;; advanced.

;;   ;; If you use Markdown or plain text files (Org renders links as buttons
;;   ;; right away)
;;   (add-hook 'find-file-hook #'denote-link-buttonize-buffer)

;;   ;; We use different ways to specify a path for demo purposes.
;;   ;; (setq denote-dired-directories
;;   ;;       (list denote-directory
;;   ;;             (thread-last denote-directory (expand-file-name "attachments"))
;;   ;;             (expand-file-name "~/Documents/books"))))

;;   ;; Generic (great if you rename files Denote-style in lots of places):
;;   ;; (add-hook 'dired-mode-hook #'denote-dired-mode)
;;   ;;
;;   ;; OR if only want it in `denote-dired-directories':
;;   (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories))

;; ;; Here is a custom, user-level command from one of the examples we
;; ;; showed in this manual.  We define it here and add it to a key binding
;; ;; below.
;; (defun my-denote-journal ()
;;   "Create an entry tagged 'journal', while prompting for a title."
;;   (interactive)
;;   (denote
;;    (denote--title-prompt)
;;    '("journal")))

;; ;; Denote DOES NOT define any key bindings.  This is for the user to
;; ;; decide.  For example:
;; (let ((map global-map))
;;   (define-key map (kbd "C-c n j") #'my-denote-journal) ; our custom command
;;   (define-key map (kbd "C-c n n") #'denote)
;;   (define-key map (kbd "C-c n N") #'denote-type)
;;   (define-key map (kbd "C-c n d") #'denote-date)
;;   (define-key map (kbd "C-c n s") #'denote-subdirectory)
;;   (define-key map (kbd "C-c n t") #'denote-template)
;;   ;; If you intend to use Denote with a variety of file types, it is
;;   ;; easier to bind the link-related commands to the `global-map', as
;;   ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
;;   ;; `markdown-mode-map', and/or `text-mode-map'.
;;   (define-key map (kbd "C-c n i") #'denote-link) ; "insert" mnemonic
;;   (define-key map (kbd "C-c n I") #'denote-link-add-links)
;;   (define-key map (kbd "C-c n b") #'denote-link-backlinks)
;;   (define-key map (kbd "C-c n f f") #'denote-link-find-file)
;;   (define-key map (kbd "C-c n f b") #'denote-link-find-backlink)
;;   ;; Note that `denote-rename-file' can work from any context, not just
;;   ;; Dired bufffers.  That is why we bind it here to the `global-map'.
;;   (define-key map (kbd "C-c n r") #'denote-rename-file)
;;   (define-key map (kbd "C-c n R") #'denote-rename-file-using-front-matter))

;; ;; Key bindings specifically for Dired.
;; (let ((map dired-mode-map))
;;   (define-key map (kbd "C-c C-d C-i") #'denote-link-dired-marked-notes)
;;   (define-key map (kbd "C-c C-d C-r") #'denote-dired-rename-marked-files)
;;   (define-key map (kbd "C-c C-d C-R") #'denote-dired-rename-marked-files-using-front-matter))

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

;; Also check the commands `denote-link-after-creating',
;; `denote-link-or-create'.  You may want to bind them to keys as well.


;; (use-package! consult-notes
;;   :config
;;   (setq consult-notes-sources '(("Org" ?o "~/stack/denote")))
;;   (consult-notes-denote-mode))

;; (require 'denote-org-dblock)


;; (map! :leader
;;       :desc "Consult Notes"
;;       "n g" #'consult-notes)

(use-package! citar-org-roam
  :config
  (setq citar-org-roam-capture-template-key "p")
  ;;(setq citar-org-roam-note-title-template "${author} - ${title}\n#+filetags: ${tags}")
  (citar-org-roam-mode))

;;(setq rascal-language-server-command
;;      "java -cp /home/arjen/Downloads/bla/rascal-language-servers/rascal-lsp/target/rascal-lsp-2.11.3-SNAPSHOT.jar:/home/arjen/.m2/repository/org/rascalmpl/rascal/0.27.3/rascal-0.27.3.jar org.rascalmpl.vscode.lsp.rascal.RascalLanguageServer")

(after! lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(".*\\.rsc$" . "rascal"))

  (setq lsp-semantic-tokens-enable t)
  (defun lsp-rascal-tcp-connect-to-port ()
    (list
     :connect (lambda (filter sentinel name _environment-fn)
                (let* ((host "localhost")
                       (port 8888)
                       (tcp-proc (lsp--open-network-stream host port (concat name "::tcp"))))

                  (set-process-query-on-exit-flag tcp-proc nil)
                  (set-process-filter tcp-proc filter)
                  (set-process-sentinel tcp-proc sentinel)
                  (cons tcp-proc tcp-proc)))
     :test? (lambda () t)))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-rascal-tcp-connect-to-port)
                    :major-modes '(rascal-mode)
                    :server-id 'rascal-lsp)))

(add-to-list 'auto-mode-alist '("\\.rsc$" . rascal-mode))

(require 'all-the-icons)
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
   ("notes" ,(list (all-the-icons-faicon "clipboard")) nil nil :ascent center)))

;; (setq org-gcal-client-id "1038002603885-7ni0fk8f5tv57iaqja2ki02eond95nf7.apps.googleusercontent.com"
;;       org-gcal-client-secret "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"
;;       org-gcal-local-timezone "Europe/Amsterdam"
;;       org-gcal-fetch-file-alist '(("0l9sq2hr7ipgb2r01u7l8o7qa4@group.calendar.google.com" .  "~/stack/Notebook/gcal-ou.org")
;;                                   ("dl8pntmql0pqrggd0r2ena6tvc@group.calendar.google.com" . "~/stack/Notebook/gcal-novi.org")
;;                                   ("family00321679463603242617@group.calendar.google.com" . "~/stack/Notebook/gcal-gezin.org")))

;; (require 'org-gcal)

(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-super-agenda-groups '((:name "Today"
                                   :time-grid t
                                   :scheduled today)
                                  (:name "Due Today"
                                   :scheduled today)
                                  (:name "Important"
                                   :priority "A")
                                  (:name "Overdue"
                                   :deadline past)
                                  (:name "Overdue scheduled"
                                   :scheduled past)
                                  (:name "Due soon"
                                   :deadline future)
                                  (:priority<= "B"
                                   :order 1)))
  (setq  org-deadline-warning-days 7
         org-agenda-breadcrumbs-separator " ❱ ")

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







;; (use-package! citar-denote
;;   :after denote
;;   :config
;;   (citar-denote-mode))

;; (map! :leader
;;       (:prefix ("d" . "Denote")
;;         "d" #'denote
;;         "r" #'denote-rename-file-using-front-matter
;;         "l" #'denote-link
;;         "c" #'citar-create-note
;;         (:prefix ("k" . "Keywords")
;;           "a" #'denote-keywords-add
;;           "r" #'denote-keywords-remove)))

(use-package! org-roam-dblocks)

(after! org
  (defun my/org-tags-view (&optional todo-only watch)
    (let ((org-use-tag-inheritance nil))
      (org-tags-view todo-only watch)))

  (advice-add 'org-tags-view :around
              (lambda (orig-fun &rest args)
                (let ((org-use-tag-inheritance nil))
                  (apply orig-fun args)))))

(use-package! ox-hugo)

(setq openai-key (getenv "OPENAI_API_KEY"))

(use-package! company-org-block
  :custom
  (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
  :hook ((org-mode . (lambda ()
                       (setq-local company-backends '(company-org-block))
                       (company-mode +1)))))

(setq chatgpt-shell-openai-key (getenv "OPENAI_API_KEY"))

(use-package! lsp-ltex
  :after lsp
  ;; :hook (text-mode . (lambda ()
  ;;                      (require 'lsp-ltex)
  ;;                      (lsp)))  ; or lsp-deferred
  :init
  (setq lsp-ltex-version "15.2.0")
  (setq lsp-ltex-check-frequency "save")

  :config

  (setq lsp-ltex-check-frequency "save")
  (add-to-list 'lsp-language-id-configuration
               '(mu4e-compose-mode . "org"))
  (add-to-list 'lsp-language-id-configuration
               '(org-msg-edit-mode . "org")))


(use-package! org-ref)

;; (use-package! base16-theme)

(after! org
  ;;  (setq org-modern-block-fringe 2)
  (setq org-modern-table nil)
  (setq org-modern-timestamp nil)
  (global-org-modern-mode)
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
                 ("\\paragraph{%s}" . "\\paragraph*{%s}"))))





;; (use-package! org-sticky-header
;;   :hook
;;   ((org-mode . (lambda () (org-sticky-header-mode))))
;;   )

;; Non-wrapping tables
(defun my/toggle-olivetti-and-line-wrap-for-org-table ()
  "Toggle olivetti-mode and line wrap when inside an Org table."
  (when
      (derived-mode-p 'org-mode)
    (if
        (org-at-table-p)
        (progn
          (setq-local truncate-lines t))
      (progn
        (setq-local truncate-lines nil)))))

(add-hook 'post-command-hook #'my/toggle-olivetti-and-line-wrap-for-org-table)
(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)

(custom-set-variables '(emojify-display-style 'unicode))


(after! chatgpt-shell
  (defun chatgpt-shell-academic-region (prefix)
    "Proofread and update the text to academic standards.

With PREFIX, invert `chatgpt-shell-insert-queries-inline' choice."
    (interactive "P")
    (chatgpt-shell-send-region-with-header
     (concat
      "As an English spelling corrector and improver, your task is to improve the structure of a provided paragraph while also correcting any spelling errors. You should should make the text fit for academics, without changing the meaning.

Your response should only include corrections and improvements to the original text. Please do not provide explanations or additional commentary. Your goal is to create a more literary version of the paragraph that maintains its original meaning but presents it in a more sophisticated manner.

Here is the text:" prefix)))

  (map! :map text-mode-map
        :localleader
        "j a" #'chatgpt-shell-academic-region)


  (map! :map TeX-mode-map
        :localleader
        "j a" #'chatgpt-shell-academic-region))






;; (after! latex-mode
;;   (map! :map LaTeX-mode-map
;;         :localleader
;;         "L c" #'langtool-check
;;         "L d" #'langtool-check-done
;;         "L s" #'langtool-switch-default-language
;;         "L m" #'langtool-show-message-at-point
;;         "L x" #'langtool-correct-buffer)
;;   )
;; (org-remark-global-tracking-mode +1)
;; (after! org-remark
;;   (defface org-remark-highlighter-yellow
;;     '((((class color) (min-colors 88) (background light))
;;        :underline "#d0bc00" :background "goldenrod1")
;;       (((class color) (min-colors 88) (background dark))
;;        :underline "#d0bc00" :background "MediumPurple4")
;;       (t
;;        :inherit highlight))
;;     "Face for the yellow highlighter pen.")

;;   ;; (defface org-remark-highlighter
;;   ;;   '((((class color) (min-colors 88) (background light))
;;   ;;      :underline "#d0bc00" :background "goldenrod1")
;;   ;;     (((class color) (min-colors 88) (background dark))
;;   ;;      :underline "#d0bc00" :background "dark magenta")
;;   ;;     (t
;;   ;;      :inherit highlight))
;;   ;;   "Face for the default highlighter pen.")

;;   (org-remark-create "yellow"
;;                      'org-remark-highlighter-yellow
;;                      '(CATEGORY "important")))


;; accept completion from copilot and fallback to company
;; (use-package! copilot
;;   :hook (prog-mode . copilot-mode)
;;   :bind (:map copilot-completion-map
;;               ("<tab>" . 'copilot-accept-completion)
;;               ("TAB" . 'copilot-accept-completion)
;;               ("C-TAB" . 'copilot-accept-completion-by-word)
;;               ("C-<tab>" . 'copilot-accept-completion-by-word)))

;; (use-package! whisper
;;   :config
;;   (setq whisper-install-directory "/tmp/"
;;         whisper-model "base"
;;         whisper-language "nl"
;;         whisper-translate nil))

(setq ispell-program-name "hunspell")
;; (use-package! lsp-tailwindcss)

(use-package! delve
  :after org-roam

  :config
  (setq delve-storage-paths "~/stack/org/"))

;; NixOS / nix use of Java together with Emacs
;;https://dschrempf.github.io/emacs/2023-03-02-emacs-java-and-nix/
(after! lsp-java
  (defun lsp-java--ls-command ()
    (list "jdt-language-server"
          "-configuration" "../config-linux"
          "-data" "../java-workspace")))

(after! cc-mode
  (defun my-set-lsp-path ()
    (setq lsp-java-server-install-dir (getenv "JDTLS_PATH")))
  (add-hook 'java-mode-hook #'my-set-lsp-path))

;; When the font patches are part of the Doom distribution then
;; this code becomes obsolete
;; https://github.com/doomemacs/doomemacs/issues/7036
(defun add-back-emoji-fallback-font-families ()
  (when (fboundp 'set-fontset-font)
    (let ((fn (doom-rpartial #'member (font-family-list))))
      (when-let (font (cl-find-if fn doom-emoji-fallback-font-families))
        (set-fontset-font t 'unicode font nil 'append)))))

(add-hook 'after-setting-font-hook 'add-back-emoji-fallback-font-families)
(add-to-list 'doom-symbol-fallback-font-families "Symbols Nerd Font")
;; end future obsolete code
