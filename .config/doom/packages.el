;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)

;; (package! benchmark-init)

;; (package! org-transclusion)

(package! modus-themes)
(package! ef-themes)

(package! spacious-padding)

;; Denote experiments
(package! denote)
(package! denote-explore)
(package! consult-notes)

(package! denote-citar
  :recipe (:type git :host github :repo "pprevos/denote-citar"))

(package! citar-org-roam)

;; (package! rascal-mode
;;   :recipe (:type git :host github :repo "bldl/rascal-emacs-mode"))
;; (package! rascal-mode
;;   :recipe (:local-repo "nursery/rascal-mode"))

;; Fix for org-roam link issue
;; (package! org :pin "ca873f7")
(package! org)

(package! org-roam-dblocks
  :recipe (:host github :repo "chrisbarrett/nursery"
           :files ("lisp/*.el")))

(package! org-drill)
;; (package! org-roam-review
;;  :recipe (:host github :repo "chrisbarrett/nursery"
;;           :files ("lisp/*.el")))

(unpin! org-roam)
(package! org-roam-ui)

(package! fancy-narrow)

(package! org-super-agenda)

(package! ox-hugo)

(package! company-org-block)

(package! shell-maker
  :recipe (:type git :host github :repo "xenodium/chatgpt-shell"))
(package! chatgpt-shell
  :recipe (:type git :host github :repo "xenodium/chatgpt-shell"))

;; (package! chatgpt
;;    :recipe (:local-repo "ai"))

(package! org-ref)

;; (package! base16-theme)

(package! org-modern)

(package! org-sticky-header)

;; (package! langtool)



;; Great package for Tiago Forte's [[id:eb517970-7045-49db-8c5b-6bef6ef1e611][Progressive Summarization]]
;; (package! org-remark)

;; (package! copilot
;;   :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

;; (package! whisper
;;   :recipe (:host github :repo "natrys/whisper.el" :files ("*.el")))

(package! org-web-tools)

(package! org-remark)


;; (package! delve
;;   :recipe (:host github :repo "publicimageltd/delve" :files ("*.el")))

;; (package! eglot-java
;;   :recipe (:host github :repo "yveszoundi/eglot-java"))

(package! all-the-icons
  :recipe (:host github :repo "domtronn/all-the-icons.el"))

(package! vulpea
  :recipe (:host github :repo "d12frosted/vulpea"))

;; (package! treesit-auto)
;; (package! templ-ts-mode)

(package! olivetti)
(package! consult)
(package! consult-denote)
