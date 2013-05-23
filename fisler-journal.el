;; this has been extensively modified by Michael Paulukonis
;; original source: http://www.emacswiki.org/cgi-bin/wiki/Journal

;; Since list people have asked for this a couple times, I thought
;; the code below belongs in a more public place.  So here it is.
;; Free, GPL'd code for whoever.
;; Enjoy,
;; ken fisler

;; To specify the directory in which to put your journal entries,
;; put the following into your ~/.emacs, specifying the directory:
;; (load "journal")
;; (if (file-directory-p "~/personal/diary/")
;;       (setq-default journal-dir "~/personal/diary/")
;; )

;; Because "format-time-string" isn't a builtin function until a later version
;; of emacs, the below won't work with this version (19.22.1).

;; Put this entire file into ".../site-lisp" or somewhere in emacs' path.

;; TODO: integrate with calendar, somehow
;; seriously, learn how to use org and planner modes
;; but this is a good learning experience, I suppose.....

(defvar journal-mode-map nil
  "Local keymap for Journal mode buffers.")

;; set up the journal mode keymap
;; with C-j to start a new journal entry (or load today's)
;; and C-S-j to search through existing entries
(if journal-mode-map
    nil
  (setq journal-mode-map (make-sparse-keymap))
  (define-key journal-mode-map "\C-jj"    'journal)
  (define-key journal-mode-map "\C-j\C-s" 'journal-search)
  (define-key journal-mode-map "\C-js"    'journal-search)	;; works just fine....
  (define-key journal-mode-map "\C-j\C-d" 'journal-divider)
  (define-key journal-mode-map "\C-j\C-n" 'journal-new-section) ;; new journal section
  (define-key journal-mode-map "\C-jn"    'journal-new-section) ;; new journal section
  (define-key journal-mode-map "\M-n"     'journal-next-section) ;; next journal section
  (define-key journal-mode-map "\M-p"     'journal-previous-section) ;; next journal section
  (define-key journal-mode-map "\C-tn"    'journal-insert-todo)
  (define-key journal-mode-map "\C-t\C-l" 'journal-done-list)
  (define-key journal-mode-map "\C-tl"    'journal-todo-list)
  (define-key journal-mode-map "\C-t\C-d" 'journal-todo-changeto-done)
  (define-key journal-mode-map "\C-td"    'journal-todo-done)
  (define-key journal-mode-map "\C-t\C-c" 'journal-todo-cancell)
  (define-key journal-mode-map "\C-tc"    'journal-todo-cancell)
  (define-key journal-mode-map "\C-jc"    'journal-clean-temps)
  (define-key journal-mode-map "\C-n"     'now)
  (define-key journal-mode-map "\C-jo"    'journal-open-journal)
)


(defun journal-mode ()
  "Journal mode. Daily journal, yadda-yadda.
Plus the following commands:
    C-j C-j  new journal entry
    C-j C-s  search (grep) existing entries.
    C-jj     journal
    C-j C-s  journal-search
    C-js     journal-search
    C-n      now
    C-j C-d  journal-divider
    C-j C-n  journal-new-section
    M-n      journal-next-section
    M-p      journal-previous-section
    C-tn     journal-insert-todo
    C-tl     journal-todo-list
    C-t C-l  journal-done-list
    C-t C-d  journal-todo-changeto-done
    C-td     journal-todo-done
    C-jc     journal-clean-temps

    these commands could be documented better.

Additionally, `allout-mode' (allout.el) has been enabled by default."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'journal-mode)
  (setq mode-name "Journal")
  (use-local-map journal-mode-map)

  (set (make-local-variable 'font-lock-defaults) '(journal-font-lock-keywords))
  (goto-address) ;;Activate URLs and e-mail addresses in the current buffer.

  (allout-mode)

;; okay, this is annoying me
;;  (setq fill-column 90) ;; 90 seems to be wide enough, but not too narrow
;;  (auto-fill-mode)

  (provide 'journal-mode)

)

(setq auto-mode-alist
      (append
       ;; File name ends in `.jnl'.
       '(("\\.jnl\\'" . journal-mode))
       auto-mode-alist)
)

;; look at this, and work with some of the date commands
;; open yesterday's journal more readily
(defun journal (filename)
  "Open .jnl file named after today's date, format YYYY-MM-DD-Day.jnl,
in subdirectory named in variable journal-dir, set in ~/.emacs,
else in $HOME."
  (interactive
   (progn

     (setq default-directory (year-month-dir))
     (setq filename (concat (format-time-string "%Y-%m-%d-%a" nil) ".jnl"))

     (list (read-file-name
        "Open journal file: " (year-month-dir) filename nil filename))
     ))

  (find-file filename) ;; switch to buffer if exists, or open file

)


(defun journal-open-journal ()
"open a journal-file, defaults to current month.
Would be nice to allow interactive selection of month and year...."
  (interactive)
  (ido-find-file)
)

(defun year-month-dir ()
"Add Year-Month dir to the defaul journal dir;
creates if does not exist."
  (interactive)
  (let ((path (concat journal-dir (format-time-string "%Y-%m" nil) "/")))
	(unless (file-directory-p path) (make-directory path t))
	path)
)


;; ;;;; commands
;; (defun umemo-save ()
;;   "Save current usage memo(annotation) into file."
;;   (interactive)
;;   (umemo-if-buffer-has-separator
;;    (let* ((path (umemo-pathname umemo-category umemo-entry-name))
;;           (dir  (file-name-directory path)))
;;      (unless (file-directory-p dir) (make-directory dir t))
;;      (write-region (point) (point-max) path)
;;      (set-buffer-modified-p nil)
;;      (message "Wrote %s" path))))


(defun now ()
  "Insert string for the current time formatted like '2:34 PM'."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%D %-I:%M %p"))
)


(defun today ()
  "Insert string for today's date nicely formatted in American style,
e.g. Sunday, September 17, 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%A, %B %e, %Y"))
)


;; Get the time exactly 24 hours from now.  This produces three integers,
;; like the current-time function.  Each integers is 16 bits.  The first and second
;; together are the count of seconds since Jan 1, 1970.  When the second word
;; increments above 6535, it resets to zero and carries 1 to the high word.
;; The third integer is a count of milliseconds (on machines which can produce
;; this granularity).  The math in the defun below, then, is to accommodate the
;; way the current-time variable is structured.  That is, the number of seconds
;; in a day is 86400.  In effect, we add 65536 (= 1 in the high word) + 20864
;; to the current-time.  However, if 20864 is too big for the low word, if it
;; would create a sum larger than 65535, then we "add" 2 to the high word and
;; subtract 44672 from the low word.

(defun tomorrow-time ()
 "*Provide the date/time 24 hours from the time now in the same format as current-time."
  (setq
   now-time (current-time)              ; get the time now
   hi (car now-time)                    ; save off the high word
   lo (car (cdr now-time))              ; save off the low word
   msecs (nth 2 now-time)               ; save off the milliseconds
   )

  (if (> lo 44671)                      ; If the low word is too big for adding to,
      (setq hi (+ hi 2)  lo (- lo 44672)) ; carry 2 to the high word and subtract from the low,
    (setq hi (+ hi 1) lo (+ lo 20864))  ; else, add 86400 seconds (in two parts)
    )
  (list hi lo msecs)                    ; regurgitate the new values
  )

;(tomorrow-time)

(defun tomorrow ()
  "Insert string for tomorrow's date nicely formatted in American style,
e.g. Sunday, September 17, 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%A, %B %e, %Y" (tomorrow-time)))
)



;; Get the time exactly 24 hours ago and in current-time format, i.e.,
;; three integers.  Each integers is 16 bits.  The first and second
;; together are the count of seconds since Jan 1, 1970.  When the second word
;; increments above 6535, it resets to zero and carries 1 to the high word.
;; The third integer is a count of milliseconds (on machines which can produce
;; this granularity).  The math in the defun below, then, is to accomodate the
;; way the current-time variable is structured.  That is, the number of seconds
;; in a day is 86400.  In effect, we subtract (65536 [= 1 in the high word] + 20864)
;; from the current-time.  However, if 20864 is too big for the low word, if it
;; would create a sum less than 0, then we subtract 2 from the high word
;; and add 44672 to the low word.

(defun yesterday-time ()
"Provide the date/time 24 hours before the time now in the format of current-time."
  (setq
   now-time (current-time)              ; get the time now
   hi (car now-time)                    ; save off the high word
   lo (car (cdr now-time))              ; save off the low word
   msecs (nth 2 now-time)               ; save off the milliseconds
   )

  (if (< lo 20864)                      ; if the low word is too small for subtracting
      (setq hi (- hi 2)  lo (+ lo 44672)) ; take 2 from the high word and add to the low
    (setq hi (- hi 1) lo (- lo 20864))  ; else, add 86400 seconds (in two parts)
    )
  (list hi lo msecs)                    ; regurgitate the new values
  )                                     ; end of yesterday-time

(defun yesterday ()
  "Insert string for yesterday's date nicely formatted in American style,
e.g. Sunday, September 17, 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%A, %B %e, %Y" (yesterday-time)))
)

(defconst divider-mark " --------------------- "
  "simple mark for dividing sections (sometime compounded).")

;; simple divider
(defun journal-divider ()
"Insert a simple dividing line, or something."
  (interactive)
  (insert "\n" divider-mark "\n\n")
)

;; if at beginning of file, there should be no initial space
(defun journal-new-section (sectionName)
  "Insert a divider with a section name"
  (interactive "sSection Name: ")
  (insert "\n\n\*" divider-mark sectionName divider-mark "\n\n")
)

;; jump to next section
;; search for the divider string
;; then drop down a line or two (if they exist)
(defun journal-next-section ()
"Jump to the next defined section."
  (interactive)
  (next-line)
  (re-search-forward (concat "\*" divider-mark ".*" divider-mark) )
)

(defun journal-previous-section ()
"Jump to the previous defined section."
  (interactive)
  (previous-line)
  (re-search-backward (concat "\*" divider-mark ".*" divider-mark) )
)

;; search the journal directories
(defun journal-search (j-regexp)
  "Seach the journal-dir for the given Regular Expression"
  (interactive "sExpression to search for: ")
  ;;(setq grep-template "grep <C> -nH -e <R> <F>")
  ;;(lgrep j-regexp "*.jnl" journal-dir)
  (setq grep-find-template "find \"<D>\" <X> -type f -name <F> -print0 | xargs -0 grep <C> -n <R> ")
  (rgrep j-regexp "*.jnl" journal-dir)
)

(defconst todo-marker "TODO: ")
(defconst done-marker "DONE: ")
(defconst cancell-marker "CANCELLED: ")

;;TODO: if using as a replacement, the date is a PITA
(defun todo-date-cancell ()
  (interactive)
  (concat cancell-marker "\(" (format-time-string "%D") " " (format-time-string "%-I:%M %p") "\) ")
)

(defun todo-date-done ()
  (interactive)
  (concat done-marker "\(" (format-time-string "%D") " " (format-time-string "%-I:%M %p") "\) ")
)


(defun journal-insert-cancell ()
  "Mark current line as a CANCELLED item (via cancell-marker)."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (insert cancell-marker)
  )
  ;; if empty line, go to end
  (if (bolp)
	  (end-of-line))
)

(defun journal-insert-todo ()
  "Mark current line as a TODO item (via todo-marker)."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (insert todo-marker)
  )
  ;; if empty line, go to end
  (if (bolp)
	  (end-of-line))
)

(defun journal-todo-insert-done ()
  "Mark current line as a DONE item (via done-marker), iff not already marked DONE:"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (search-forward done-marker nil t)
	nil
      (insert (todo-date-done))
    )
    (if (bolp)
	(end-of-line))
    )
  )

(defun journal-todo-changeto-done ()
  "Changes mark of TODO item on current line as DONE (via done-marker)."
  (interactive)
  (save-excursion
    (let ((beg (progn (beginning-of-line) (point)))
	  (end (progn (end-of-line) (point))))
	  (replace-regexp todo-marker (todo-date-done) nil beg end)
	  )
    )
)

;; (narrow-to-region (re-search-backward "^") (re-search-forward "$"))


(defun journal-todo-cancell ()
  "Changes mark of TODO item on currnt line to DONE, or inserts DONE on this line."
  (interactive)
  (save-excursion
    ;; compare to beg/end assignments above
    ;; this is shorter -- the above is more obvious
    ;; well, until you know the emacs regexp better
    ;; hrm....
    (narrow-to-region (re-search-backward "^") (re-search-forward "$"))
    (beginning-of-line)
    (if (or (search-forward todo-marker nil t)
	    (search-forward done-marker nil t)
	    (search-forward cancell-marker nil t))
	(replace-match (todo-date-cancell))
      (journal-insert-cancell)
      )
    )
  (if (bolp)
      (end-of-line))
  (widen) ;; pull back from narrowing
  )

(defun journal-todo-done ()
  "Changes mark of TODO item on currnt line to DONE, or inserts DONE on this line."
  (interactive)
  (save-excursion
    ;; compare to beg/end assignments above
    ;; this is shorter -- the above is more obvious
    ;; well, until you know the emacs regexp better
    ;; hrm....
    (narrow-to-region (re-search-backward "^") (re-search-forward "$"))
    (beginning-of-line)
    (if (search-forward todo-marker nil t)
	(replace-match (todo-date-done))
      (journal-todo-insert-done)
      )
    )
  (if (bolp)
      (end-of-line))
  (widen) ;; pull back from narrowing
  )

(defun journal-todo-list ()
  "Searches the journal-files for lines beginning with todo-marker."
  (interactive)
  (journal-search (concat "^" todo-marker))
)

(defun journal-done-list ()
  "Searches the journal-file for lines beginning with the DONE marker"
  (interactive)
  (journal-search (concat "^" done-marker))
)


(defun journal-clean-temps ()
"clean up temp (\"~\") files in journal-directory"
  (interactive)
  (dired journal-dir)
  (revert-buffer)  ;;make sure buffer is current
  (dired-flag-backup-files)
  (dired-do-flagged-delete t)
  ;; and now dired asks for verification, and the user needs to enter "yes"
  ;; dang
  ;; how to get around this?
  ;; maybe dired-mark-pop-up ???
  (quit-window)
)

;; http://two-wugs.net/emacs/mode-tutorial.html
;; wow. this works. pick something brighter
;; also, keep on looking...
(defconst journal-font-lock-keywords
  (list
   '("\\(TODO:\\)" . font-lock-comment-face)
   '("\\(DONE:\\)" . font-lock-builtin-face)
   '("\\(CANCELLED:\\)" . font-lock-builtin-face)
   (cons divider-mark '(. font-lock-warning-face))
   )
  "Minimal highlighting expressions for journal mode")
