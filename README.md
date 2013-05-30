# A simple elisp journal

Based on original code found at [EmacsWiki:Journal](http://www.emacswiki.org/emacs/Journal).

I used this just-about daily from about 2008-2013, switching to org-mode in May, 2013.

I always planned on releasing the code back into the wild while I was using it, and see no reason (beyond embarassment) to not do so now.

I still hope to enhance the code a bit -- fix some issues, rip out some broken pieces, and add prefix-spacing (based on the [elisp coding conventions](http://www.gnu.org/software/emacs/manual/html_node/elisp/Coding-Conventions.html)).

# configuration
(this should be in a separate .el file eventually)

In the below example, `dropbox-site-lisp` is exactly what you might think it is.
And it should be replaced with `\path\to\your\journal.el`

    ;; work with Personal Journal
    (defun edit-journal ()
      "Load the journal.el file automatically."
      (interactive)
      (find-file (concat dropbox-site-lisp "journal.el"))
    )

    (load "journal")
    (setq journal-dir "~/Personal/Journal/")
    (add-to-list 'auto-mode-alist '("\\.jnl\\'" . journal-mode))


