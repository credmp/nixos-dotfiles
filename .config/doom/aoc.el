;;; tools to work with aoc-cli

;; Requires:
;; aoc-cli: https://github.com/scarvalhojr/aoc-cli
;; markdown-mode

(require 'auth-source)
;; Argument 1 will be the year, argument 2 the day

(defcustom aoc-year 2018
  "The year of the AoC competition to track"
  :group 'aoc :type '(integer))

(defcustom aoc-day 1
  "The day of the year of the AoC competition to track"
  :group 'aoc :type '(integer))

(defcustom aoc-part 1
  "The part of the day of the year of the AoC competition to track"
  :group 'aoc :type '(integer))

(defcustom aoc-input-file-format "/home/arjen/Projects/Clojure/aoc-%1$d/resources/input-%2$02d-real.txt"
  "The file path where puzzle inputs will be written to. The string will be
passed to the `format' function using the arguments `aoc-year' and `aoc-day'."
  :group 'aoc :type 'number)


;; When a day is finished it will progress to the next on
;; (customize-save-variable 'aoc-day 9)
;; (setq aoc-day 25)
;; (message (format "day %d year %d" (if (= 25 aoc-day)
;;                                       (let ()
;;                                         (setq aoc-year (+ 1 aoc-year))
;;                                         1)
;;                                     (+ 1 aoc-day)) aoc-year))

(defun aoc/progress-part ()
  "Progress a part of an AoC puzzle"
  (if (= aoc-part 2)
      (aoc/increase-aoc-day)
    (progn
      (customize-save-variable 'aoc-part (+ 1 aoc-part))
      t)))

(defun aoc/increase-aoc-day ()
  "Progress to the next day of AOC. Reset the part to 1 and if the day was 25,
roll over until the next year."
  (if (< aoc-day 25)
      (progn
        (customize-save-variable 'aoc-day (+ 1 aoc-day))
        (customize-save-variable 'aoc-part 1)
        t)
    (progn
      (customize-save-variable 'aoc-year (+ 1 aoc-year))
      (customize-save-variable 'aoc-day 1)
      (customize-save-variable 'aoc-part 1)
      t)))


;; (aoc/progress-part)
;; (aoc/increase-aoc-day)

(defun aoc/current-progress ()
  "Show where we are at"
  (interactive)
  (message (format "Advent of Code: Year %d Day %d Part %d" aoc-year aoc-day aoc-part)))


(defun aw/aoc-update-puzzle (prefix)
    "Retrieve the description for an advent of code puzzle. Use a prefix
  to retrieve for a specific year or day. The input is inserted into the current
  buffer, a new buffer will be made with the description of the puzzle."
    (interactive "p")
    (let* ((year  (cond ((= prefix 1)
                         (string-to-number (format-time-string "%Y")))
                        ((> prefix 1)
                         (read-number "Year: "))))
           (day (cond ((= prefix 1)
                       (string-to-number (format-time-string "%d")))
                      ((> prefix 1)
                       (read-number "Day: "))))
           (creds (auth-source-search :host "adventofcode.com"))
           (secret (plist-get (car creds) :secret))
           (decoded (funcall secret))
           (url-request-extra-headers `(("Cookie" . ,(concat "session=" decoded)))))
      (with-output-to-temp-buffer "aoc-puzzle"
        (pop-to-buffer "aoc-puzzle")
        (url-insert-file-contents (format "https://adventofcode.com/%d/day/%d" year day))
        (read-only-mode -1)
        (goto-char 1)
        (while (search-forward-regexp "\\(.*\n\\)+?\\(<article\\(.*\n\\)+</article>\\)+\\(.*\n\\)*" nil t) 
          (replace-match (match-string 2) t nil))
        (shr-render-region (point-min) (point-max)))))
        
      

(defun aoc/puzzle-input (year day)
    "Retrieve the description for an advent of code puzzle. Use a prefix
  to retrieve for a specific year or day. The input is inserted into the current
  buffer, a new buffer will be made with the description of the puzzle."
    (interactive "p")
    (let* ((creds (auth-source-search :host "adventofcode.com"))
           (secret (plist-get (car creds) :secret))
           (decoded (funcall secret))
           (file (format aoc-input-file-format year day))
           (url-request-extra-headers `(("Cookie" . ,(concat "session=" decoded)))))
      (make-directory (file-name-directory file) t)
      (find-file file)
      (url-insert-file-contents (format "https://adventofcode.com/%d/day/%d/input" year day))
      (save-buffer)
      (kill-buffer)
      (message "Puzzle input written to %s" file)))
      

(aoc/puzzle-input 2018 2)
;; When a day is finished it will progress to the next on
;; (customize-save-variable 'aoc-day (+ 1 aoc-day))
;; (setq aoc-day 25)
;; (message (format "day %d year %d" (if (= 25 aoc-day)
;;                                       (let ()
;;                                         (setq aoc-year (+ 1 aoc-year))
;;                                         1)
;;                                     (+ 1 aoc-day)) aoc-year))


;; Status 200 OK
;; <main>
;; <article><p>That's not the right answer.  If you're stuck, make sure you're using the full input data; there are also some general tips on the <a href="/2019/about">about page</a>, or you can ask for hints on the <a href="https://www.reddit.com/r/adventofcode/" target="_blank">subreddit</a>.  Please wait one minute before trying again. (You guessed <span style="white-space:nowrap;"><code>1</code>.)</span> <a href="/2019/day/1">[Return to Day 1]</a></p></article>
;; </main>


;; Answer too fast, still 200 OK
;; <main>
;; <article><p>You gave an answer too recently; you have to wait after submitting an answer before trying again.  You have 46s left to wait. <a href="/2019/day/1">[Return to Day 1]</a></p></article>
;; </main>

;; (aoc/current-progress)

;; (aoc/progress-part)
;; (aoc/increase-aoc-day)

;;(setq aoc-input-file-format "/home/arjen/Projects/Java/competitive-programming/aoc-%1$d/src/test/resources/input-%2$02d.txt")

(defun aoc/get-puzzle ()
  "Retrieve the puzzle description for day and year"
  (interactive)
  (let ((process "*aoc-cli*")
        (buffer "*aoc*")
        (day (format "--day=%d" (if aoc-day aoc-day 1)))
        (year (format "--year=%d" (if aoc-year aoc-year 2016))))
    ;;(apply #'start-process process buffer "aoc" "r" args)
    (apply #'call-process "aoc" nil buffer nil "r" (list day year))
    (switch-to-buffer buffer)
    (goto-char (point-min))
    (markdown-view-mode)))

(defun aoc/get-input ()
  "Retrieve the puzzle input for day and year and insert it into the current buffer"
  (interactive)
  (let* ((file (format aoc-input-file-format year day))
         (day (format "--day=%d" (if aoc-day aoc-day 1)))
         (year (format "--year=%d" (if aoc-year aoc-year 2016)))
         (text (shell-command-to-string (format  "aoc d %s %s --file %s" day year file))))
    (message  (format "Good luck! %s" text))))

(defun aoc/submit (part &optional args)
  (interactive (list (read-number "Part: ")
                     (transient-args 'test-transient)))

  (message (format  "Part %d %s" part args)))
  

;; (aoc/get-puzzle)

;; (aoc/get-input-old 2021 9)

;; (format aoc-input-file-format 2016 17)

;; Transient UI

(defun test-function (&optional args)
  (interactive
   (list (transient-args 'test-transient)))
  (message "args %s" args))



(define-transient-command test-transient ()
       "Test Transient Title"
       ["Arguments"
        ("-d" "Day" "--day=")
        ("-y" "Year" "--year=")]
         
       ["Actions"
        ("d" "Download puzzle input" test-function)
        ("r" "Read description" aoc/get-puzzle)
        ("s" "Submit answer" aoc/submit)])

;; (test-transient)

(require 'transient)

(defclass argh--variable (transient-variable)
  ((scope       :initarg :scope)))

(define-infix-command argh-set-query ()
  "Set the `query' variable in the source buffer."
  :class 'argh--variablex
  :key "-q"
  :argument "--query="
  :variable 'query)

(define-transient-command argh-transient ()
  "Show transient for current buffer."
  ["Query"
   (argh-set-query)])

(cl-defmethod transient-infix-set ((obj argh--variable) value)
  "Set a variable."
  (let ((variable (oref obj variable)))
    (oset obj value value)
    (set (make-local-variable (oref obj variable)) value)
    (unless (or value transient--prefix)
      (message "Unset %s" variable))))



(require 'cl-lib)

(transient-define-suffix pmx-show-prefix ()
  "Show the prefix that invoked this suffix"
  :description "prefix"
  (interactive)
  (message "Current prefix key: %s" transient-current-prefix))

(transient-define-suffix pmx-show-command ()
  "Show this command"
  :description "current command"
  (interactive)
  (message "Current command: %s" transient-current-command))

(transient-define-suffix pmx-show-suffixes ()
  "Show the current suffixes"
  :description "suffixes"
  (interactive)
  (message "Current suffixes: %s" (cl-mapcar
                                   (lambda (obj)
                                     (oref obj description))
                                   transient-current-suffixes)))

(transient-define-suffix pmx-show-args ()
  "Show current infix args"
  :description "infix args"
  (interactive)
  (message "Current infix args: %s" (transient-args transient-current-command)))

(transient-define-suffix pmx-send-message ()
  "Send message to minibuffer"
  :description "send message"
  :transient t
  (interactive)
  (message "Message sent at %s. Happy?" (shell-command-to-string "echo -n $(date)")))

(transient-define-argument pmx-affirmative ()
  "Are we affirmative?"
  :description "affirmative"
  :argument "affirmative")

(transient-define-argument pmx-yep-nope ()
  "Is it yep or is it nope?"
  :description "yep or nope"
  :class 'transient-option
  :shortarg "-y"
  :argument "--yepnope="
  :choices '("yep" "nope"))

(transient-define-argument pmx-abc ()
  "Which letters do you like?"
  :description "abc"
  :class 'transient-option
  :shortarg "-a"
  :argument "--abc="
  :choices '("A" "B" "C"))

(defvar pmx--variable "A string" "A variable brought to you by pmx")

(defcustom pmx--variable "abc"
  "Special variable for test"
  :group 'aoc :type 'string)


(transient-define-argument pmx-set-lisp-variable ()
  "Set a lisp variable, pmx--variable.  Won't show up in infix arguments."
  :description "set pmx--variable"
  :class 'transient-lisp-variable
  :shortarg "-l"
  :variable 'pmx--variable
  :argument "--letters=")

(transient-define-suffix pmx-show-lisp-variable ()
  "Access pmx--variable"
  :description "show pmx--variable"
  (interactive)
  (message "Current value of pmx--variable: %s" pmx--variable))

(transient-define-suffix pmx-dynamic-suffix ()
  "Description depends on pmx--variable"
  :if-not '(lambda () (string-equal pmx--variable "abc"))
  :description '(lambda () (format "pmx %s" pmx--variable))
  (interactive)
  (message "Current value of pmx--variable: %s" pmx--variable))

(transient-define-prefix pmx-nested-transient ()
  "Some subcommands, like tree menus from the land of mice"
  ["Switches"
   ("-s" "another switch" ("-x" "--conflicting"))]
  ["Sub Command Introspection"
   ("i" pmx-show-args)
   ("p" pmx-show-prefix)
   ("s" pmx-show-suffixes)
   ("c" pmx-show-command)]
  ["Dynamic Commands"
   ("d" pmx-dynamic-suffix)])

(transient-define-prefix pmx-transient-toy ()
  "Figure out how to use transient's API properly"
  [:class transient-columns
   ["Things"
    ("-w" "switch"  ("-w" "--switch"))]
   ["Others"
    ("i" pmx-show-args)
    ("p" pmx-show-prefix)
    ("s" pmx-show-suffixes)
    ("c" pmx-show-command)
    ("m" pmx-send-message)]
   ["More"
    ("f" pmx-affirmative)
    ("y" pmx-yep-nope)
    ("a" pmx-abc)
    ("l" pmx-set-lisp-variable)
    ("w" pmx-show-lisp-variable)]
   ["Drilldown"
    ("d" "drilldown" pmx-nested-transient)]])

(global-set-key (kbd "M-o") 'pmx-transient-toy)



