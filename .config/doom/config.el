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
;; (setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 12)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 13))

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
(setq org-directory "~/Projects/Notebook/")
(setq org-roam-directory "~/Projects/Notebook/")

(after! org

  (setq! org-capture-templates '(("b" "Blog idea" entry (file+olp "~/Nextcloud/roam-new/20231008105247-planning.org" "Inbox" "Series")
                                  "* %?\n%T" :prepend t)
                                 ("t" "todo" entry
                                  (file+headline "~/Nextcloud/roam-new/20231008105247-planning.org" "Inbox")
                                  "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
                                 ("T" "Tickler" entry
                                  (file+headline "~/Nextcloud/roam-new/20231008105247-planning.org" "Inbox")
                                  "* %i%? \n %U")
                                 ("w" "Web site" entry
                                  (file "")
                                  "* %a :website:\n\n%U %?\n\n%:initial")
                                 ("wN" "Web link" entry
                                  (file+headline ,(car org-agenda-files)
                                                 "Links to read later")
                                  "* TODO [#A]  %?%a \nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"Fri\"))\n"
                                  :immediate-finish t :empty-lines 1)
                                 ("e" "email" entry (file+headline "~/Nextcloud/roam-new/20231008105247-planning.org" "Inbox")
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
        :where (like tag (quote "%\"planner\"%"))]))))
  ;;(aw/gather-agenda-files))
  ;; depends on the function from org definition above


  (defun vulpea-agenda-files-update (&rest _)
    "Update the value of `org-agenda-files'."
    (setq org-agenda-files (vulpea-project-files))))



(use-package! org
  :after vulpea
  :custom
  ;; populate org-agenda-files using vulpea, which queries org-roam
  (setq org-agenda-files #'(vulpea-project-files))
  (add-hook 'before-save-hook #'vulpea-project-update-tag)
  (advice-add 'org-agenda :before #'vulpea-agenda-files-update)
  (advice-add 'org-todo-list :before #'vulpea-agenda-files-update))


;; -- Nursery projects
;; git clone git@github.com:chrisbarrett/nursery.git nursery
(use-package! org-roam-dblocks
  :hook (org-mode . org-roam-dblocks-autoupdate-mode))


;; (use-package! lsp-java
;;   :after lsp)
;; :config
;; (require 'lsp-java-boot))
;; (setq lsp-java-vmargs
;;       '("-XX:+UseParallelGC"
;;         "-XX:GCTimeRatio=4"
;;         "-Dsun.zip.disableMemoryMapping=true"
;;         "-noverify"
;;         "-Xmx1G"
;;         "-XX:+UseG1GC"
;;         "-XX:+UseStringDeduplication"
;;         ,(concat "-javaagent:" ;; probably need to update this.
;;                  (expand-file-name "/Users/arjen/.m2/repository/org/projectlombok/lombok/1.18.42/lombok-1.18.42.jar"))
;;         ,(concat "-Xbootclasspath/a:"
;;                  (expand-file-name "/Users/arjen/.m2/repository/org/projectlombok/lombok/1.18.42/lombok-1.18.42.jar"))))


;; :hook
;; (
;;  ;; (lsp-mode-hook . #'lsp-java-lens-mode)
;;  ;;(java-mode-hook . #'lsp-java-boot-lens-mode)
;;  )))

(use-package! eglot-java
  :config
  (setq lombok-library-path "~/.m2/repository/org/projectlombok/lombok/1.18.42/lombok-1.18.42.jar")
  (setq lombok-arg  (concat "-javaagent:" (expand-file-name lombok-library-path)))
  (setq eglot-java-eclipse-jdt-args
        '("-Xmx4G" "--add-modules=ALL-SYSTEM" "--add-opens" "java.base/java.util=ALL-UNNAMED" "--add-opens" "java.base/java.lang=ALL-UNNAMED"
          "-javaagent:/home/arjen/.m2/repository/org/projectlombok/lombok/1.18.42/lombok-1.18.42.jar"))
  (add-hook 'java-ts-mode-hook 'eglot-java-mode)
  (add-hook 'java-mode-hook 'eglot-java-mode))
;; (after! lsp-java
;;   ;; (setq lombok-library-path (concat doom-data-dir "lombok.jar"))
;;   (setq lombok-library-path "~/.m2/repository/org/projectlombok/lombok/1.18.42/lombok-1.18.42.jar")


;;   ;; (unless (file-exists-p lombok-library-path)
;;   ;;   (url-copy-file "https://projectlombok.org/downloads/lombok.jar" lombok-library-path))

;;   (setq lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx4G" "-Xms100m"))

;;   (push (concat "-javaagent:"
;;                 (expand-file-name lombok-library-path)) ;; (expand-file-name lombok-library-path)
;;         lsp-java-vmargs))

;; (use-package! denote
;;   :config
;;   (require 'denote-journal-extras)
;;   (setq denote-directory (expand-file-name "~/Nextcloud/denote/"))
;;   (setq denote-journal-extras-title-format 'day-date-month-year)
;;   (setq denote-journal-extras-directory (expand-file-name "~/Nextcloud/denote/20-29 Life Admin/21 Mental Health/21.01 Daily Journal"))
;;   (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories))

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(setq evil-want-fine-undo t)

(setq ispell-program-name "hunspell")

(use-package! gptel)
;; :config
;; (setq gptel-model 'devstral:latest)
;; (setq gptel-backend (gptel-make-ollama "Ollama"
;;                       :host "localhost:11434"
;;                       :stream t
;;                       :models '(devstral:latest)))

(map! :leader
      (:prefix-map ("e" . "gptel")
                   (:desc "gptel menu" "m" #'gptel-menu
                    :desc "gptel rewrite" "r" #'gptel-rewrite)))

;; (map! :leader
;;       (:prefix-map ("s" . "search")
;;                    (:desc "imenu" "i" #'lsp-ui-imenu)))

(use-package! citar
  :custom
  (citar-bibliography '("~/Nextcloud/My-Library.bib")))

;; (require 'hydra)

;; (defhydra lsp-clojure-refactor-menu (:color blue :hint nil)
;;   "
;; Threading                      Code Manip                      Namespace                       Misc
;; ------------------------------------------------------------------------------------------------------------------------------------------------------
;; _th_: Thread first             _el_: Expand let                _cn_: Clean ns                  _cp_: Cycle privacy
;; _tf_: Thread first all         _il_: Introduce let             _am_: Add missing libspec       _cc_: Cycle coll
;; _tt_: Thread last              _ml_: Move to let
;; _tl_: Thread last all          _ef_: Extract function
;; _ua_: Unwind all               _rn_: Rename
;; _uw_: Unwind thread
;; "

;;   ("cp" lsp-clojure-cycle-privacy)
;;   ("cn" lsp-clojure-clean-ns)
;;   ("cc" lsp-clojure-cycle-coll)
;;   ("am" lsp-clojure-add-missing-libspec)
;;   ("el" lsp-clojure-expand-let)
;;   ("il" lsp-clojure-introduce-let)
;;   ("ef" lsp-clojure-extract-function)
;;   ("ml" lsp-clojure-move-to-let)
;;   ("th" lsp-clojure-thread-first)
;;   ("rn" lsp-rename)
;;   ("tf" lsp-clojure-thread-first-all)
;;   ("tt" lsp-clojure-thread-last)
;;   ("tl" lsp-clojure-thread-last-all)
;;   ("ua" lsp-clojure-unwind-all)
;;   ("uw" lsp-clojure-unwind-thread))
;; (map! :map clojure-mode-map
;;       :localleader
;;       :desc "refactor" "R" #'lsp-clojure-refactor-menu/body)

(after! epa
  (setq! epa-pinentry-mode 'loopback))

;; (setq! lsp-disabled-clients '(ruby-ls rubocop-ls typeprof-ls))

;; (map! :leader
;;       :desc "Avy activate lens"
;;       "c y" #'lsp-avy-lens)

(after! projectile
  (setq! projectile-create-missing-test-files t))

(use-package! denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :config
  (setq denote-directory (expand-file-name "~/Projects/Notebook/"))
  (denote-rename-buffer-mode 1))

(map! :leader
      (:prefix ("d" . "denote")
       :desc "Denote" "d" #'denote
       :desc "Rename Denote File" "r" #'denote-rename-file
       :desc "Denote Link" "l" #'denote-link
       :desc "Denote Backlinks" "b" #'denote-backlinks
       :desc "Denote Dired" "d" #'denote-dired
       :desc "Denote Grep" "g" #'denote-grep
       :desc "Denote subdirectory" "s" #'denote-subdirectory))

(use-package! gptel-aibo
  :after (gptel flycheck)
  :config
  (map! :map prog-mode-map
        :localleader
        :desc "AIbo" "a" #'gptel-aibo))
;; (use-package! flyover
;;   :hook ((flycheck-mode . flyover-mode))
;;   :config
;;   (setq flyover-levels '(error warning info))
;;   (setq flyover-use-theme-colors t)
;;   ;; (setq flyover-virtual-line-icon "— ") ;;; default its nil
;;   (setq flyover-virtual-line-icon "╰—→") ;;; default its nil

;;   ;; Setting this to t would show the message at end of line instead of below
;;   ;; In that case I would recommend changing to something like
;;   ;; (setq flyover-virtual-line-icon "→ ") ;;; default its nil
;;   (setq flyover-show-at-eol nil))




(setq flymake-show-diagnostics-at-end-of-line 'fancy)

;; (use-package! better-org-habit)

(use-package! magit-gitflow
  :hook (magit-mode . 'turn-on-magit-gitflow))

(use-package! asdf-vm)


;; Java stuff
(defun get-classpath ()
  "Generate and retrieve the classpath for the current project using Maven."
  (let* ((project-root (projectile-project-root))
         (pom-file (expand-file-name "pom.xml" project-root))
         (classpath-file "/tmp/cp.txt"))
    (when (or (not (file-exists-p classpath-file))
              (time-less-p (nth 5 (file-attributes classpath-file))
                           (nth 5 (file-attributes pom-file))))
      (let ((default-directory project-root))
        (shell-command (concat "./mvnw dependency:build-classpath -Dmdep.outputFile=" classpath-file "> /dev/null 2>&1"))))
    (with-temp-buffer
      (insert-file-contents classpath-file)
      (buffer-string))))

(defun ts-get-current-java-method-name ()
  "Return the current Java method name at point using Tree-sitter."
  (interactive)
  (let* ((current-node (treesit-node-at (point)))
         (method-node (treesit-node-parent current-node)))
    (while (and method-node
                (not (member (treesit-node-type method-node) '("method_declaration" "method_definition"))))
      (setq method-node (treesit-node-parent method-node)))
    (if (and method-node (treesit-node-named-p method-node))
        (let ((method-name (treesit-node-text method-node)))
          (message "Current method: %s" method-name))
      (message "No method found."))))

(defun run-junit-for-current-class ()
  "Run JUnit tests for the current Java class using the JUnit console."
  (interactive)
  (let* ((project-root (projectile-project-root))
         (classpath (get-classpath))
         (class-name (concat (if (fboundp '+java-current-package)
                                 (+java-current-package)
                               (error "Package function not available"))
                             "."
                             (if (fboundp '+java-current-class)
                                 (+java-current-class)
                               (error "Class function not available"))))
         (command (concat "java -XX:+EnableDynamicAgentLoading -jar "
                          "/home/arjen/.config/doom/junit-platform-console-standalone.jar "
                          "execute "
                          "-cp " classpath ":" project-root "/target/classes:" project-root "/target/test-classes "
                          "--select-class " class-name)))

    (let ((output-buffer (get-buffer-create "*JUnit Output*")))
      ;; Use compilation-start to set up the compilation window
      (compilation-start command 'compilation-mode)
      (with-current-buffer output-buffer
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert (format "Running command: %s\n\n" command))
        (setq buffer-read-only t)))))
(require 'treesit)

(defun my/get-current-function-name ()
  "Return the name of the tree-sitter function or method at point.
Returns nil if not inside a function."
  (interactive)
  ;; First, ensure tree-sitter is actually active in this buffer.
  (let* (;; Define the node types that represent functions/methods.
         (function-types '(function_definition method_definition function_declaration method_declaration))
         ;; Define the field names that typically hold the function's name.
         (name-fields '("name" "identifier"))
         ;; Find the enclosing function node by walking up the tree.
         (function-node (treesit-parent-while (treesit-node-at (point))
                                              (lambda (n) (not (memq (treesit-node-type n) function-types))))))
    ;; If we found a function node...
    (when function-node
      ;; ...try to find its name node using the list of possible field names.
      (let (name-node)
        (while (and (not name-node) name-fields)
          (setq name-node (treesit-node-child-by-field-name function-node (car name-fields)))
          (setq name-fields (cdr name-fields)))
        ;; If a name node was found, extract and return its text.
        (when name-node
          (treesit-node-text name-node))))))

(defun my/show-current-function-name ()
  "Show the current function/method name in the echo area."
  (interactive)
  (let ((name (my/get-current-function-name)))
    (if name
        (message "Current function: %s" name)
      (message "Not inside a recognized function or method."))))

(use-package! xclip
  :config
  (setq xclip-program "wl-copy")
  (setq xclip-select-enable-clipboard t)
  (setq xclip-mode t)
  (setq xclip-method (quote wl-copy)))
