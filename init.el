;;
;; emacs configuration
;;
;; Made by mefyl <mefyl@lrde.epita.fr>
;;
;; Based on Nicolas Despres <despre_n@lrde.epita.fr> configuration
;; Thanks go to Micha <micha@lrde.epita.fr> for his help
;; Enhanced by vincent <lechem_v@yaka.epita.fr>
;; Enhanced by Thomas Badie <badie@lrde.epita.fr>
;;

;; TODO
;;
;;

;; Nice function to do not forget:
;; align-regexp: take a region, a regexp and align your regexp foreach line.
;; c-x v l: print the last log message.
;; c-x v =: make a diff with the commited version.
;; multi-occur*: Highlight all occurence.
;; c-x d: Open dired.
;;      R: rename a file
;;      d: mark to delete
;;      x: execute the deletion
;;      C: copy
;;      =: Make a diff.
;; M-j: In a comment, go to the next line, open a comment, and indent.
;; M-@: Make a region around the next word.
;; M-- M-@: Make a region around the previous word.
;; C-M-@: Mark the next sexp.

;; Funny
;;   (un)morse-region
;;   rot13-region
;;   butterfly


(defconst xemacs (string-match "XEmacs" emacs-version)
  "non-nil iff XEmacs, nil otherwise")



;; CUSTOM FUNCTIONS


;; Compilation

(setq compile-command "make")

(defun build ()
  (interactive)
  (if (string-equal compile-command "")
    (progn
      (while (not (or (file-readable-p "Makefile") (file-readable-p "Drakefile") (string-equal (cwd) "/")))
        (cd ".."))
      (if (string-equal (cwd) "/")
        (message "No Makefile or Drakefile found.")
          (if (file-readable-p "Makefile")
            (compile (concat "cd " (cwd) " && make"))
            (compile (concat "cd " (cwd) " && drake")))))
    (recompile)))


;; Edition

(defun c-switch-hh-cc ()
  (interactive)
  (let ((other
         (let ((file (buffer-file-name)))
           (if (string-match "\\.hh$" file)
               (replace-regexp-in-string "\\.hh$" ".cc" file)
             (if (string-match "\\.hxx$" file)
                 (replace-regexp-in-string "\\.hxx" ".cc" file)
               (replace-regexp-in-string "\\.cc$" ".hh" file))))))
    (find-file other)))

(defun c-switch-hh-hxx-cc ()
  (interactive)
  (let ((other
         (let ((file (buffer-file-name)))
           (if (string-match "\\.hh$" file)
               (replace-regexp-in-string "\\.hh$" ".hxx" file)
             (if (string-match "\\.cc$" file)
                 (replace-regexp-in-string "\\.cc$" ".hxx" file)
               (replace-regexp-in-string "\\.hxx$" ".hh" file))))))
    (find-file other)))

(defun count-word (start end)
  (let ((begin (min start end))(end (max start end)))
    (save-excursion
      (goto-char begin)
      (re-search-forward "\\W*") ; skip blank
      (setq i 0)
      (while (< (point) end)
        (re-search-forward "\\w+")
        (when (<= (point) end)
          (setq i (+ 1 i)))
        (re-search-forward "\\W*"))))
  i)
(defun stat-region (start end)
  (interactive "r")
  (let*
      ((words (count-word start end))
       (lines (count-lines start end))
       (c-lines (count-comments-line start end))
       (e-lines (count-empty-line start end))
       (norme-line (- lines (+ c-lines e-lines))))
    (message
     (concat "Lines: "
             (int-to-string lines)
             "  Words: "
             (int-to-string words)
             "  Comments lines: "
             (int-to-string c-lines)
             "  Empty lines: "
             (int-to-string e-lines)
             "  Norme lines: "
             (int-to-string norme-line)))
    )
  )

(defun ruby-command (cmd &optional output-buffer error-buffer)
  "Like shell-command, but using ruby."
  (interactive (list (read-from-minibuffer "Ruby command: "
					      nil nil nil 'ruby-command-history)
		          current-prefix-arg
			       shell-command-default-error-buffer))
  (shell-command (concat "ruby -e '" cmd "'") output-buffer error-buffer))

;; Shell

(defun cwd ()
  (replace-regexp-in-string "Directory " "" (pwd)))

(defun insert-shell-shebang ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert "#! /bin/sh\n\n")))

;; New badie_t
;; This function insert the perl shebang, and ask to the system
;; for where is perl
(defun insert-perl-shebang ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert "#! ")
    (insert (shell-command-to-string "which perl"))
    (insert "\nuse strict;\n\n")
    (goto-char (point-min))
    (end-of-line)
    (insert " -w")
    (goto-char (point-max))))


;; C/C++


(defun get-dir-name ()
  (let ((where (file-name-directory buffer-file-name)))
     (file-name-nondirectory (del-last-char where))))

(defun insert-header-guard ()
  (interactive)
  (save-excursion
    (when (buffer-file-name)
        (let*
            ((dirname (get-dir-name))
             (name_ (file-name-nondirectory buffer-file-name))
             (name (concat dirname "." name_))
             (macro_ (replace-regexp-in-string
                     "\\." "_"
                     (replace-regexp-in-string
                      "-" "_"
                      (upcase name))))
             (macro (if (string-match "spot" buffer-file-name)
                        (concat "SPOT_" macro_ "_")
                      (concat macro_ "_"))))
          (goto-char (point-min))
          (insert "#ifndef " macro "\n")
          (insert "# define " macro "\n\n")
          (goto-char (point-max))
          (insert "\n#endif // !" macro "\n")))))

(defun insert-header-inclusion ()
  (interactive)
  (when (buffer-file-name)
    (let
        ((name
          (replace-regexp-in-string ".c$" ".h"
          (replace-regexp-in-string ".cc$" ".hh"
          (file-name-nondirectory buffer-file-name)))))
      (insert "#include \"" name "\"\n\n"))))


(defun sandbox ()
  "Opens a C++ sandbox in current window."
  (interactive)
  (cd "/tmp")
  (let ((file (make-temp-file "/tmp/" nil ".cc")))
    (find-file file)
    (insert "int main()\n{\n\n}\n")
    (line-move -2)
    (save-buffer)
    (compile (concat "g++ -W -Wall -I /usr/include/qt4/ -I /usr/include/qt4/QtCore/ -L /usr/lib/qt4 -lQtCore " file " && ./a.out"))))

(defun rbegin ()
  (min (point) (mark)))

(defun rend ()
  (max (point) (mark)))

;; Generic FIXME insertion method. Works as soon as the mode posseses
;; a comment-region function.
(defun insert-fixme (&optional msg)
  (interactive "sFixme: ")
  (save-excursion
    (end-of-line)
    (when (not (looking-back "^\s*"))
      (insert " "))
    (setq start (point))
    (insert "FIXME")
    (when (not (string-equal msg ""))
      (insert ": " msg))
  (comment-region start (point))))

(defun insert-end-of-fix (&optional msg)
  (interactive "sEnd of fix: ")
  (save-excursion
    (end-of-line)
    (when (not (looking-back "^\s*"))
      (insert " "))
    (setq start (point))
    (insert "EOFX")
    (when (not (string-equal msg ""))
      (insert ": " msg))
  (comment-region start (point))))

(defun c-insert-include (name &optional r)
  (interactive "sInclude: \nP")
  (save-excursion
    (beginning-of-line)
    (when (not (looking-at "\W*$"))
      (insert "\n")
      (line-move -1))
    (insert "#include ")
    (if r
        (insert "<>")
      (insert "\"\""))
    (backward-char 1)
    (insert name)))

(defun c-insert-debug (&optional msg)
  (interactive)
  (when (not (looking-at "\W*$"))
    (beginning-of-line)
    (insert "\n")
    (line-move -1))
  (c-indent-line)
  (insert "std::cout << \"\" << std::endl;")
  (backward-char 15))

(defun c-insert-block (&optional r b a)
  (interactive "P")
  (unless b (setq b ""))
  (unless a (setq a ""))
  (if r
      (progn
        (save-excursion
          (goto-char (rbegin))
          (beginning-of-line)
          (insert "\n")
          (line-move -1)
          (insert b "{")
          (c-indent-line))
        (save-excursion
          (goto-char (- (rend) 1))
          (end-of-line)
          (insert "\n}" a)
          (c-indent-line)
          (line-move -1)
          (end-of-line))
        (indent-region (rbegin) (rend)))
    (progn
        (beginning-of-line)

        (setq begin (point))

        (insert b "{\n")
        (end-of-line)
        (insert "\n}" a)

        (indent-region begin (point))

        (line-move -1)
        (end-of-line))))

(defun c-insert-braces (&optional r)
  (interactive "P")
  (c-insert-block r))

(defun c-insert-ns (name r)
  (interactive "sName: \nP")
  (c-insert-block r (concat "namespace " name "\n")))

(defun c-insert-switch (value r)
  (interactive "sValue: \nP")
  (c-insert-block r (concat "switch (" value ")\n")))

(defun c-insert-if (c r)
  (interactive "sCondition: \nP")
  (c-insert-block r (concat "if (" c ")\n")))

(defun c-insert-class (name)
  (interactive "sName: ")
  (c-insert-block () (concat "class " name "\n") ";")
  (insert "public:")
  (c-indent-line)
  (insert "\n")
  (c-indent-line))


(defun beamer-insert-frame (name)
  (interactive "sName: ")
  (insert (concat "\\begin{frame}{" name "}\n\\end{frame}"))
  (line-move -1)
  (end-of-line))

(defun beamer-insert-block (name)
  (interactive "sName: ")
  (insert (concat "\\begin{block}{" name "}\n\\end{block}"))
  (line-move -1)
  (end-of-line))

;; OPTIONS

(setq inhibit-startup-message t)        ; don't show the GNU splash screen
(setq frame-title-format "%b")          ; titlebar shows buffer's name
(global-font-lock-mode t)               ; syntax highlighting
(setq font-lock-maximum-decoration t)   ; max decoration for all modes
(setq transient-mark-mode 't)           ; highlight selection
(setq line-number-mode 't)              ; line number
(setq column-number-mode 't)            ; column number

(progn
  (scroll-bar-mode nil)               ; no scroll bar
  (menu-bar-mode nil)                 ; no menu bar
  (tool-bar-mode nil)                 ; no tool bar
  (mouse-wheel-mode t))


(setq delete-auto-save-files t)         ; delete unnecessary autosave files
(setq delete-old-versions t)            ; delete oldversion file
(setq-default normal-erase-is-backspace-mode t) ; make delete work as it should
(fset 'yes-or-no-p 'y-or-n-p)           ; 'y or n' instead of 'yes or no'
(setq default-major-mode 'text-mode)    ; change default major mode to text
(setq ring-bell-function 'ignore)       ; turn the alarm totally off
(setq make-backup-files nil)            ; no backupfile


; Column in the modeline.
(setq column-number-mode t)
; The mouse wheel affect the cursor.
(setq mouse-wheel-follow-mouse t)
; tab is 2 space.
(setq tab-width 2)
; Indentation is two by two.
(setq standard-indent 2)
; Highlight the region
(transient-mark-mode 1)
; Do not beep.
(setq visible-bell t)
; Show the corresponding parenthesis.
(setq show-paren-face 'modeline)
;; Color and revert
(setq global-font-lock-mode t)
(setq global-auto-revert-mode t)

(setq user-full-name "Thomas Badie")
(setq user-mail-address "thomas.badie@epita.fr")
(setq user-string "Thomas Badie  <thomas.badie@epita.fr>")
(setq smtpmail-default-smtp-server "smtp.epita.fr")
(setq send-mail-function 'smtpmail-send-it)
(load-library "smtpmail")

(setq delete-auto-save-files t) ;no ~


;; FIXME: wanted 99.9% of the time, but can cause your death 0.1% of
;; the time =). Todo: save buffer before reverting
;(global-auto-revert-mode t)            ; auto revert modified files

;(pc-selection-mode)                    ; selection with shift
(auto-image-file-mode)                  ; to see picture in emacs
;(dynamic-completion-mode)              ; dynamic completion
(when (string-match "^22." emacs-version)
  (ido-mode t))
(show-paren-mode t); match parenthesis
(setq-default indent-tabs-mode nil); don't use fucking tabs to indent

;; HOOKS

; Delete trailing whitespaces on save
(add-hook 'write-file-hooks 'delete-trailing-whitespace)
 '(ido-ignore-buffers (quote ("*Gnus*")))
 '(ido-ignore-buffers (quote ("*Mail*")))

; Auto insert C/C++ header guard
(add-hook 'find-file-hooks
	    (lambda ()
              (when (and (memq major-mode '(c-mode c++-mode)) (equal (point-min) (point-max)) (string-match ".*\\.hh?" (buffer-file-name)))
                (insert-header-guard)
                (goto-line 3)
                (insert "\n"))))

(add-hook 'find-file-hooks
	    (lambda ()
	          (when (and (memq major-mode '(c-mode c++-mode)) (equal (point-min) (point-max)) (string-match ".*\\.cc?" (buffer-file-name)))
		          (insert-header-inclusion))))

(add-hook 'sh-mode-hook
          (lambda ()
            (when (equal (point-min) (point-max))
              (insert-shell-shebang)
              (goto-char (point-max)))))

;; New badie_t:
;; I want to have automatic perl shebang, warning, and strict
;; when I open a perl file.
(add-hook 'find-file-hooks
	    (lambda ()
	          (when (and (memq major-mode '(cperl-mode perl-mode)) (equal (point-min) (point-max)) (string-match ".*\\.pl" (buffer-file-name)))
              (insert-perl-shebang)
              (goto-char (point-max)))))

; Start code folding mode in C/C++ mode
(add-hook 'c-mode-common-hook (lambda () (hs-minor-mode 1)))


;; Ruby

(add-to-list 'load-path "/usr/share/emacs/site-lisp/ruby-mode/")
(autoload 'ruby-mode "ruby-mode"
 "Mode for editing ruby source files" t)

;; file extensions
(add-to-list 'auto-mode-alist '("\\.l$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.y$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ll$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.yy$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hh$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.xcc$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.xhh$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.pro$" . sh-mode)) ; Qt .pro files
(add-to-list 'auto-mode-alist '("configure$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Drakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.sed$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.tig$" . tiger-mode))
(add-to-list 'auto-mode-alist '("\\.tih$" . tiger-mode))
(add-to-list 'auto-mode-alist '("TODO$" . org-mode))

;; Font

(defconst default-font-size 120)

(defun set-font-size (&optional size)
  "Set the font size to SIZE (default: default-font-size)."
  (interactive "nSize: ")
  (unless size
    (setq size default-font-size))
  (set-face-attribute 'default nil :height size))

(set-face-attribute 'default nil :height 100)

(defun reset-font-size ()
  (interactive)
  (set-font-size))

(defun find-next (c l)
    (if (< c (car l))
        (car l)
      (if (cdr l)
          (find-next c (cdr l))
        (car l))))

(defun find-prev (c l &optional ac)
    (if (or (not l) (<= c (car l)))
        (if ac
            ac
          (car l))
      (find-prev c (cdr l) (car l))))

(defun current-font-size ()
  (face-attribute 'default :height))

(defconst font-sizes '(60 75 90 105 120 135 170 280))

(defun inc-font-size ()
  (interactive)
  (let ((new (find-next (current-font-size) font-sizes)))
    (set-font-size new)
    (message (concat "New font size: " (int-to-string new)))))

(defun dec-font-size ()
  (interactive)
  (let ((new (find-prev (current-font-size) font-sizes ())))
    (set-font-size new)
    (message (concat "New font size: " (int-to-string new)))))

(set-font-size)

;; Ido

;; tab means tab, i.e. complete. Not "open this file", stupid.
(setq ido-confirm-unique-completion t)
;; If the file doesn't exist, do try to invent one from a transplanar
;; directory. I just want a new file.
(setq ido-auto-merge-work-directories-length -1)

;; Don't switch to GDB-mode buffers
;; (add-to-list 'ido-ignore-buffers "locals")
(setq ido-ignore-buffers (quote ("\\‘\\*breakpoints of.*\\*\\’"
  "\\‘\\*stack frames of.*\\*\\’" "\\‘\\*gud\\*\\’""\\‘\\*locals of.*\\*\\’"
  "\\‘ ")))


;; Better gdb
(setq-default gdb-many-windows t)

;; BINDINGS

(global-set-key [(control backspace)] 'backward-kill-word)
(global-set-key [(delete)] 'delete-char)
(global-set-key [(control delete)] 'kill-word)
;; (global-set-key [(control h)] 'delete-backward-char) ;; ssh fix



;; BINDINGS :: font

(global-set-key [(control +)] 'inc-font-size)
(global-set-key [(control -)] 'dec-font-size)
(global-set-key [(control =)] 'reset-font-size)

;; BINDINGS :: windows

(global-unset-key [(control s)])
(global-set-key [(control s) (v)] 'split-window-horizontally)
(global-set-key [(control s) (h)] 'split-window-vertically)
(global-set-key [(control s) (d)] 'delete-window)
(global-set-key [(control s) (o)] 'delete-other-windows)
(global-set-key [(control s) (i)] 'windmove-up)
(global-set-key [(control s) (j)] 'windmove-left)
(global-set-key [(control s) (k)] 'windmove-down)
(global-set-key [(control s) (l)] 'windmove-right)


(defun buffer-exists (bufname)
  (not (eq nil (get-buffer bufname))))

(defun print_buffer ()
  (interactive)
  (if (buffer-exists "*ansi-term*")
      (message "found")
    (message "not found")))


(require 'term)
;; If we do not set these variables, we have a prompt in black on white, it was horrible.
(setq term-default-bg-color "black")
(setq term-default-fg-color "white")

;; badie_t new:
;; Now f7 means "open an ansi-term which runs zsh. Don't matter where it is"
(global-set-key [(f7)] '(lambda (&optional r)
                          (interactive "P")
                          (if r
                              (let ((where (shell-command-to-string "which zsh")))
                                (ansi-term (substring where 0 (1- (length where)))))
                            (if (buffer-exists "*ansi-term*")
        ;                        (if (term-check-proc (buffer-name))
                                    (switch-to-buffer "*ansi-term*")
;                                  (let ((where (shell-command-to-string "which zsh")))
;                                    (ansi-term (substring where 0 (1- (length where))))))
                              (let ((where (shell-command-to-string "which zsh")))
                                (ansi-term (substring where 0 (1- (length where)))))))))


;; BINDINGS :: ido
(global-set-key [(control b)] 'ido-switch-buffer)

;; BINDINGS :: isearch
(global-set-key [(control f)] 'isearch-forward-regexp)  ; search regexp
(global-set-key [(control r)] 'query-replace-regexp)    ; replace regexp
(define-key
  isearch-mode-map
  [(control n)]
  'isearch-repeat-forward)                              ; next occurence
(define-key
  isearch-mode-map
  [(control p)]
  'isearch-repeat-backward)                             ; previous occurence
(define-key
  isearch-mode-map
  [(control z)]
  'isearch-cancel)                                      ; quit and go back to start point
(define-key
  isearch-mode-map
  [(control f)]
  'isearch-exit)                                        ; abort
(define-key
  isearch-mode-map
  [(control r)]
  'isearch-query-replace)                               ; switch to replace mode
(define-key
  isearch-mode-map
  [S-insert]
  'isearch-yank-kill)                                   ; paste
(define-key
  isearch-mode-map
  [(control e)]
  'isearch-toggle-regexp)                               ; toggle regexp
(define-key
  isearch-mode-map
  [(control l)]
  'isearch-yank-line)                                   ; yank line from buffer
(define-key
  isearch-mode-map
  [(control w)]
  'isearch-yank-word)                                   ; yank word from buffer
(define-key
  isearch-mode-map
  [(control c)]
  'isearch-yank-char)                                   ; yank char from buffer


;; BINDINGS :: Latex
;; (define-key
;;   latex-mode-map
;;   [(control c) (control g)]
;;   'beamer-insert-frame)

;; BINDINGS :: Lisp

(define-key
  lisp-mode-map
  [(control c) (control f)]
  'insert-fixme)                                      ; insert fixme

(define-key
  lisp-mode-map
  [(control c) (control e)]
  'insert-end-of-fix)                                      ; insert fixme

;; BINDINGS :: Ruby

;(define-key
;  ruby-mode-map
;  [(control c) (control f)]
;  'insert-fixme)                                      ; insert fixme

; (require 'php-mode)

;; BINDINGS :: C/C++

(require 'cc-mode)

(define-key
  c-mode-base-map [f5] 'compile)
(define-key
  c-mode-base-map [f6] 'gdb)
(define-key
  c-mode-base-map [(control c) (a)] 'add-change-log-entry-other-window)
(define-key
  c-mode-base-map
  [(control c) (p)]
  'pdf-moisi)                                      ; switch between .hh and .cc
(define-key
  c-mode-base-map
  [(control c) (w)]
  'c-switch-hh-cc)                                      ; switch between .hh and .cc
(define-key
  c-mode-base-map
  [(control c) (q)]
  'c-switch-hh-hxx-cc)                                      ; switch between .hxx and .cc
(define-key
  c-mode-base-map
  [(control c) (f)]
  'hs-hide-block)                                       ; fold code
(define-key
  c-mode-base-map
  [(control c) (s)]
  'hs-show-block)                                       ; unfold code
(define-key
  c-mode-base-map
  [(control c) (control s)]
  'c-insert-switch)                                     ; insert switch
(define-key
  c-mode-base-map
  [(control c) (control i)]
  'c-insert-if)                                         ; insert if
(define-key
  c-mode-base-map
  [(control c) (control b)]
  'c-insert-braces)                                     ; insert braces
(define-key
  c-mode-base-map
  [(control c) (control f)]
  'insert-fixme)                                        ; insert fixme
(define-key
  c-mode-base-map
  [(control c) (control d)]
  'c-insert-debug)                                      ; insert debug
(define-key
  c-mode-base-map
  [(control c) (control l)]
  'c-insert-class)                                      ; insert class
(define-key
  c-mode-base-map
  [(control c) (control i)]
  'c-insert-include)                                    ; insert include

(define-key
  c-mode-base-map
  [(control c) (control s)]
  'replace-string)

;; ;; BINDINGS :: C/C++ :: XRefactory

;; (define-key
;;   c-mode-base-map
;;   [(control c) (d)]
;;   'xref-push-and-goto-definition)                       ; goto definition
;; (define-key
;;   c-mode-base-map
;;   [(control c) (b)]
;;   'xref-pop-and-return)                                 ; go back
;; (define-key
;;   c-mode-base-map
;;   [C-return]
;;   'xref-completion)                                     ; complete

;; BINDINGS :: sgml

(require 'sgml-mode)

(define-key
  html-mode-map
  [(control c) (control c)]
  'comment-region)

;; BINDINGS :: misc

(global-set-key [(meta =)]
                'stat-region)
(global-set-key [(control c) (control c)]
                'comment-or-uncomment-region)
(if (display-graphic-p)
    (global-set-key [(control z)] 'undo))                ; undo only in graphic mode

(global-set-key [(control c) (control a)] 'mark-whole-buffer)       ; select whole buffer
(global-set-key [(control return)] 'dabbrev-expand)     ; auto completion
(global-set-key [C-home] 'beginning-of-buffer)          ; go to the beginning of buffer
(global-set-key [C-end] 'end-of-buffer)                 ; go to the end of buffer
(global-set-key [(meta g)] 'goto-line)                  ; goto line #
(global-set-key [A-left] 'windmove-left)                ; move to left windnow
(global-set-key [A-up] 'windmove-up)                    ; move to right window
(global-set-key [A-down] 'windmove-down)                ; move to upper window
(global-set-key [A-right] 'windmove-right)
(global-set-key (kbd "M-<left>") 'windmove-left)                ; move to left windnow
(global-set-key (kbd "M-<down>") 'windmove-down)                ; move to upper window
(global-set-key (kbd "M-<up>") 'windmove-up)                ; move to upper window
(global-set-key (kbd "M-<right>") 'windmove-right)                ; move to upper window
(global-set-key [(control c) (c)] 'build)
(global-set-key [(control c) (d)] 'gen-doxy-com)
(global-set-key [(control c) (e)] 'next-error)
(global-set-key [(control tab)] 'other-window)          ; Ctrl-Tab = Next buffer
(global-set-key [(control backtab)]
                '(lambda () (interactive)
                   (other-window -1)))                  ; Ctrl-Shift-Tab = Previous buffer
(global-set-key [(control delete)]
                'kill-word)                             ; kill word forward
(global-set-key [(meta ~)] 'ruby-command)               ; run ruby command

;; ;; COLORS

(load-file "~/.emacs.d/plugins/zenburn.el")
(require 'zenburn)
(color-theme-zenburn)


;; Qt

(font-lock-add-keywords 'c++-mode
			'(("foreach\\|forever\\|emit" . 'font-lock-keyword-face)))

;; Lisp mode

(require 'lisp-mode)

(define-key
  lisp-mode-shared-map
  [(control c) (control c)]
  'comment-region)                                      ; comment


;; C / C++ mode

(require 'cc-mode)
(add-to-list 'c-style-alist
             '("epita"
               (c-basic-offset . 2)
               (c-comment-only-line-offset . 0)
               (c-hanging-braces-alist     . ((substatement-open before after)))
               (c-offsets-alist . ((topmost-intro        . 0)
                                   (substatement         . +)
                                   (substatement-open    . 0)
                                   (case-label           . +)
                                   (access-label         . -)
                                   (inclass              . ++)
                                   (inline-open          . 0)))))

(setq c-default-style "epita")

;; Compilation

(setq compilation-window-height 12)
(setq compilation-scroll-output t)

;; make C-Q RET insert a \n, not a ^M

(defadvice insert-and-inherit (before ENCULAY activate)
  (when (eq (car args) ?
)
    (setcar args ?\n)))


(prefer-coding-system 'utf-8)

;; Sessions

(desktop-load-default)
(desktop-read)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(after-save-hook (quote (executable-make-buffer-file-executable-if-script-p)))
 '(ecb-options-version "2.40")
 '(ido-auto-merge-work-directories-length -1)
 '(ido-confirm-unique-completion t)
 '(ido-create-new-buffer (quote always))
 '(ido-everywhere t)
 '(ido-ignore-buffers (quote ("\\`\\*breakpoints of.*\\*\\'" "\\`\\*stack frames of.*\\*\\'" "\\`\\*gud\\*\\'" "\\`\\*locals of.*\\*\\'" "\\` ")))
 '(ido-mode (quote both) nil (ido))
 '(mail-signature t)
 '(require-final-newline t)
 '(rst-level-face-base-color "grey")
 '(rst-level-face-base-light 30)
 '(show-paren-mode t nil (paren))
 '(show-paren-style (quote parenthesis))
 '(speedbar-frame-parameters (quote ((minibuffer) (width . 20) (border-width . 0) (menu-bar-lines . 0) (tool-bar-lines . 0) (unsplittable . t) (set-background-color "black")))))

;; Uniquify is useful when having two file with the same name in two directory.
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(setq-default ispell-program-name "aspell")

(setq-default gdb-many-windows t)

;; Save and restore window layout
(defvar winconf-ring ())

(defun push-winconf ()
  (interactive)
  (window-configuration-to-register ?%)
  (push (get-register ?%) winconf-ring))

(defun pop-winconf ()
  (interactive)
  (set-register ?% (pop winconf-ring))
  (jump-to-register ?%))

(defun restore-winconf ()
  (interactive)
  (set-register ?% (car winconf-ring))
  (jump-to-register ?%))

(if (file-exists-p "~/.d_myemacs/")
    (setq load-path (cons (concat "~/.d_myemacs/") load-path)))

(defun my-load-file (file)
  (if (file-exists-p file)
      (load-file file)))

(if (>= emacs-major-version 21)
  (setq selection-coding-system 'compound-text-with-extensions))

;; delete the selected text on insert
(delete-selection-mode 1)

(global-set-key [(control x) (k)] 'kill-this-buffer)

(if (< emacs-major-version 22)
  (progn

    (defun yic-ignore (str)
      (or
        (string-match "\\*Buffer List\\*" str)
        (string-match "^TAGS" str)
        (string-match "^\\*Messages\\*$" str)
        (string-match "^\\*Completions\\*$" str)
        (string-match "^ " str)

        (memq str
          (mapcar
            (lambda (x)
              (buffer-name
                (window-buffer
                 (frame-selected-window x)
                )
              )
            )
            (visible-frame-list)
          )
        )
      )
    )

    (defun yic-next (ls)
      "Determiner le prochain tampon valide a afficher"
      (let* ((ptr ls)
              bf bn go
            )
        (while (and ptr (null go))
          (setq bf (car ptr)  bn (buffer-name bf))
          (if (null (yic-ignore bn))
            (setq go bf)
            (setq ptr (cdr ptr))
          )
        )
        (if go (switch-to-buffer go))
      )
    )

    (defun yic-prev-buffer ()
      "Afficher le tampon suivant en ignorant ceux non souhaites"
      (interactive)
      (yic-next (reverse (buffer-list)))
    )

    (defun yic-next-buffer ()
      "Afficher le tampon precedent en ignorant ceux non souhaites"
      (interactive)
      (bury-buffer (current-buffer))
      (yic-next (buffer-list))
    )

    (global-set-key [(control x) (control left)] 'yic-prev-buffer)
    (global-set-key [(control x) (control right)] 'yic-next-buffer)
  )
)


(my-load-file "~/.emacs.d/plugins/pdfmoisi.el")
(my-load-file "~/.emacs.d/plugins/myheader.el")
(my-load-file "~/.emacs.d/plugins/hh2cc.el")
(my-load-file "~/.emacs.d/plugins/gen-doxy-com.el")
(my-load-file "~/.emacs.d/plugins/cmake-mode.el")
(my-load-file "~/.emacs.d/plugins/rebox.el")
(my-load-file "~/.emacs.d/plugins/tiger.el")
(my-load-file "~/.emacs.d/plugins/lua-mode.el")

(add-hook 'lua-mode-hook 'turn-on-font-lock)
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)


(setq auto-mode-alist
        (append
            '(("CMakeLists\\.txt\\'" . cmake-mode))
               '(("\\.cmake\\'" . cmake-mode))
                  auto-mode-alist))

(autoload 'cmake-mode "~/CMake/Docs/cmake-mode.el" t)



;; highlight trailing whitespace
;; up badie_t:
;; Don't highlight them when you are in an ansi-term.
(add-hook 'find-file-hook
          '(lambda ()
             (when (and (not (memq major-mode '(term-mode))) (not (string-match ".*ansi-term.*" (buffer-name))))
               (setq show-trailing-whitespace t))))

; Set line number
(global-linum-mode t)
(setq linum-format "%3d ")

(global-hi-lock-mode 1)

;; Change the name of the emacs window.
(setq frame-title-format
      '("emacs%@" (:eval (system-name))
        ": " (:eval (if (buffer-file-name)
                        (abbreviate-file-name (buffer-file-name))
                  "%b")) " [%*]"))



;; Test: Using CEDET really great?
;; It is!
(my-load-file "~/.emacs.d/plugins/cedet/cedet-1.0/common/cedet.el")
;(global-ede-mode 1)
(semantic-load-enable-minimum-features)
(require 'semantic-ia)
(require 'semantic-gcc)

;(require 'semanticdb)
;; (require 'semanticdb-global)
;; (semanticdb-enable-gnu-global-databases 'c-mode)
;; (semanticdb-enable-gnu-global-databases 'c++-mode)

(add-to-list 'load-path "~/.emacs.d/git/anything-config")
(add-to-list 'load-path "~/.emacs.d/git/anything-config/extensions")
(require 'anything-config)
;; (require 'anything-gtags)

(defun my-anything ()
  (interactive)
  (anything-other-buffer
   '(anything-c-source-file-name-history
     anything-c-source-info-pages
     anything-c-source-locate
     anything-c-source-emacs-commands)
   " *my-anything*"))

;(setq anything-gtags-enable-initial-pattern t)

;; Coming from
;; http://emacs-fu.blogspot.com/2009/11/copying-lines-without-selecting-them.html
;; New way to copy/cut:
;; We decorate M-w and C-w in the aim to work on the current
;; line if there is no region selected.
;; defadvice is a elisp macro to do this kind of decoration (documentation:
;; http://www.gnu.org/software/emacs/elisp/html_node/Advising-Functions.html)
(defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-end-position)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-end-position)))))


(defun perl-command (cmd &optional output-buffer error-buffer)
  (interactive
   (list
    (read-from-minibuffer "Perl command: " nil nil nil 'perl-command-history)
    current-prefix-arg
    shell-command-default-error-buffer))
  (shell-command (concat "perl -e '" cmd "'") output-buffer error-buffer))

;; Another useful cut! Cut from the cursor to the beginning of the line.
;; Function already exists, I just bind it.
(global-set-key (kbd "M-k") (lambda () (interactive) (kill-line 0)))

;; A good way to navigate through the kill ring.
;; It open a new frame with all the things that have been cut.
;; You have just to choose the one you want.
;; This function is in the package `emacs-goodies-el'
;;(global-set-key (kbd "M-y") (lambda () (interactive) (browse-kill-ring)))
;; A better way is to use anything. Anything allow to browse through the kill-ring
;; and apply pattern matching. But there is an annoying thing which is fixed by this:
;; The problem with `anything-show-kill-ring' is: It close the window
;; after the choice. I think a right way to make it better is to add a
;; `save-window-excursion' before. So here is my add-on. I use an advice because
;; this is exactly what I want to do.
(defadvice anything-show-kill-ring (around activate compile)
  (save-window-excursion
    ad-do-it))

;; I just have to bind it.
(global-set-key (kbd "M-y") (lambda () (interactive) (anything-show-kill-ring)))



;; Now we help us with emacs tags. What the hell is this?
;; It is a way to navigate from the source file, to the declaration.
;; Or from the declaration, to every utilisation of the symbol.
;; Allow to go faster!
;; Epiteen, we need to go fast if we want to sleep =)

;; How it works: When you are on a function call,
;; M-. will tell you where the function is defined. (If there is some
;; function with the same name, he shows everything. Else he go to
;; the definition).
(add-to-list 'load-path "~/.emacs.d/plugins")
(autoload 'gtags-mode "gtags" "" t)

(add-hook 'c-mode-hook '(lambda() (gtags-mode 1)))

;; badie_t:
;; This function is coming from the emacs-fu blog.
;; I modified it because it happens that I open a draft file, and I don't want
;; to select tags for a simple `hello world' program.
;; So, if there is already tags for a file, they will be updated at init, and
;; if we want to create tags, we must call this function.
;; Update:
;; I don't want to wait the end of the process on tags before edit files.
;; The first idea was to run asynchronously the shell command, but a ugly window
;; pop up! I found on the net and I get the (save-window-excursion ... ) part.
;; I does exactly what I want.

;; (defun gtags-create-or-update (&optional ask-for-create)
;;   "create or update the gnu global tag file"
;;   (interactive "P")
;;   (if (not
;;        (= 0 (call-process "global" nil nil nil " -p"))) ; tagfile doesn't exist?
;;       (if ask-for-create
;;         (let ((olddir default-directory)
;;               (topdir (read-directory-name
;;                        "gtags: top of source tree:" default-directory)))
;;           (cd topdir)
;;           (save-window-excursion
;;             (shell-command "if ! test `pgrep gtags`; then { gtags && echo 'created tagfile'; } &
;; fi"))
;;           (cd olddir))
;;         (interactive "Tags does not exists for this file.")) ; restore
;;     ;;  tagfile already exists; update it
;;     (save-window-excursion
;;       (shell-command "if ! test `pgrep gtags`; then { gtags && echo 'created tagfile'; } &
;; fi")))); old: { global -u && echo 'updated tagfile'; } &
;; ;; I don't know why, if I didn't put a newline, the shell yell at me:
;; ;; /bin/bash: -c: line 0: syntax error near unexpected token `;'
;; ;; /bin/bash: -c: line 0: `if ! test `pgrep gtags`; then { gtags && echo 'created tagfile'; } &; fi'

;; (add-hook 'gtags-mode-hook
;;   (lambda()
;;     (local-set-key (kbd "M-.") '(lambda (&optional r)
;;                                   (interactive "P")
;;                                   (if r
;;                                       (gtags-find-symbol)
;;                                     (gtags-find-tag))))   ; find a tag, also M-.
;;     (local-set-key (kbd "M-,") 'gtags-find-rtag)))  ; reverse tag

;; (add-hook 'c-mode-common-hook
;;   (lambda ()
;;     (require 'gtags)
;;     (gtags-mode t)
;;     (gtags-create-or-update)))

;; When I use bookmarks, I don't want to put special files on my ~.
(setq bookmark-default-file "~/.emacs.d/data/bookmarks") ;; bookmarks

;; For ascii art lovers.
;; Select a region, and apply figlet-region. If it is installed on your system,
;; all the text in the region is transformed in an ascii-art and it is
;; commented.
(defun figlet-region (&optional b e)
  (interactive "r")
  (let ((where (shell-command-to-string "which figlet")))
    (if (and (not (string-equal where ""))
        (not (string-match ".*found.*" where)))
        (figlet-region-core b e)
      (message "figlet is not installed"))))

(defun figlet-region-core (&optional b e)
  (save-excursion
    (shell-command-on-region b e "figlet" t)
    (comment-region b (point))
    (delete-trailing-whitespace)))

(defun del-last-char (where)
  (substring where 0 (1- (length where))))

;; Want to count non-empty lines, and non only comments line.
(defun count-empty-line (start end)
  (let ((begin (min start end))(end (max start end)))
    (save-excursion
      (goto-char begin)
      (setq j 0)
      (while (< (point) end)
        (re-search-forward "^$")
        (when (<= (point) end)
          (setq j (+ 1 j)))
        (next-line))))
      j)

(defun count-comments-line (start end)
  (let ((begin (min start end))(end (max start end)))
    (save-excursion
      (goto-char begin)
      (setq j 0)
      (while (and (< (point) end)
                  (re-search-forward "^[ \t]*//.*" end t))
        (when (<= (point) end)
          (setq j (+ 1 j)))
        (next-line))))
      j)


;; When I put my emacs in fullscreen, it hides the clock.
;; Today I realized that I changed the workspace only to see it...
;; Now it is fully include in my emacs :)
(require 'time)
(setq display-time-24hr-format 1)
(display-time-mode 1)

;; Trying to use recentf. Save a list of recently open file.
;; Seems to be cool. Keep it and see how I use it.
(require 'recentf)
(recentf-mode 1)

(global-set-key "\C-x\C-r" (lambda() (interactive)
    (anything
     :prompt "Switch to: "
     :candidate-number-limit 10                 ;; up to 10 of each
     :sources
     '( anything-c-source-buffers               ;; buffers
        anything-c-source-recentf               ;; recent files
        anything-c-source-bookmarks             ;; bookmarks
        anything-c-source-files-in-current-dir+ ;; current dir
        anything-c-source-locate))))            ;; use 'locate'

;; Tonight I test diff-buffer-with-file. It could be useful, so I bind it :)
(global-set-key "\C-s\C-d" 'diff-buffer-with-file)

;; I am not satisfied by the way to modify the size of the windows.
;; Let's try to hack it!
(require 'windmove)

(defun window-exists-dir (dir)
    (let ((other-window (windmove-find-other-window dir)))
      (cond ((null other-window)
              nil)
            ((and (window-minibuffer-p other-window)
                  (not (minibuffer-window-active-p other-window)))
             nil)
            (t
             t))))

(defun push-left-window-separator ()
  (interactive)
  (if (window-exists-dir 'left)
      (enlarge-window-horizontally 3)
    (shrink-window-horizontally 3)))

(defun push-right-window-separator ()
  (interactive)
  (if (window-exists-dir 'right)
      (enlarge-window-horizontally 3)
    (shrink-window-horizontally 3)))

(defun push-up-window-separator ()
  (interactive)
  (if (window-exists-dir 'up)
      (enlarge-window 3)
    (shrink-window 3)))

(defun push-down-window-separator ()
  (interactive)
  (if (window-exists-dir 'down)
      (enlarge-window 3)
    (shrink-window 3)))

;; This hack is pretty useful, but very limited. In fact, it does not
;; work as we expect in the case of more than two window on the same line.
;; Need to find a better way to say "enlarge" or "reduce".


;; keybindings:
(global-set-key (kbd "C-M-<left>") 'push-left-window-separator)
(global-set-key (kbd "C-M-<right>") 'push-right-window-separator)
(global-set-key (kbd "C-M-<up>") 'push-up-window-separator)
(global-set-key (kbd "C-M-<down>") 'push-down-window-separator)
;; Succeed!

;; The three following function are stolen from
;; http://keypod.net/wordpress/2008/08/30/emacs-kill-matching-buffers-by/
;; It makes exactly the work I want on buffers' name.
;; I make my add to delete special mode.
(defun act-on-buffers (pred action)
  (let ((count 0))
    (dolist(buffer (buffer-list))
      (when (funcall pred buffer)
        (setq count (1+ count))
        (funcall action buffer)))
    count))

(defun kill-matching-buffers-by (acc re)
  "Kill buffers whose name matches the input"
  (let*
      ((match (function
               (lambda (buf)
                 (string-match re (funcall acc buf)))))
       (kill #'(lambda (buf) (kill-buffer buf)))
       (count (act-on-buffers match kill)))
    count))

(defun kill-matching-buffers-by-name (re)
  (interactive)
  (kill-matching-buffers-by 'buffer-name re))

(defun kill-matching-buffers-by-mode (re)
  (interactive)
  (kill-matching-buffers-by 'get-mode re))

;; This function kills several kind of buffers created by the use of
;; tags, or completion, shell command, dired... All those things
;; you don't want to see when you're looking for a buffer.
(defun clear-dirty-window()
  (interactive)
  (let ((countd 0))
    (setq countd (+ countd (kill-matching-buffers-by-name "gtags")))
    (setq countd (+ countd (kill-matching-buffers-by-name "\\*shell command output\\*")))
    (setq countd (+ countd (kill-matching-buffers-by-name "completions\\*")))
    (setq countd (+ countd (kill-matching-buffers-by-name "background")))
    (setq countd (+ countd (kill-matching-buffers-by-mode "dired-mode")))
  (message "%d buffer removed" countd)))

;; I want to use it quickly.
(global-set-key [(f8)] 'clear-dirty-window)

(defun print-mode (buf)
  "Function to know the mode where the current buffer is."
  (interactive "sBuffer: ")
  (message (get-mode buf)))

;; The only way I found to get the major mode as string, is to redirect
;; the output in a string.
;; i use with-current-buffer to make emacs believe the current buffer is buf.
(defun get-mode (buf)
  (with-output-to-string
    (with-current-buffer buf
      (princ major-mode))))

;; A new way to navigate through buffer!
;; When you have too many buffer, sometimes it means you have several group
;; of buffers: *scratch* *message* etc... Project1... Project2...
;; Now you can separate them into group! =)
(require 'ibuffer)
(my-load-file "~/.config/emacs/filter.el")

(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "default")))

;; I want the cursor on the most recent buffer
(defadvice ibuffer (around ibuffer-point-to-most-recent) ()
    "Open ibuffer with cursor pointed to most recent buffer name"
    (let ((recent-buffer-name (buffer-name)))
      ad-do-it
      (ibuffer-jump-to-buffer recent-buffer-name)))
  (ad-activate 'ibuffer)

;; I don't want to see empty group
(setq ibuffer-show-empty-filter-groups nil)

;; I don't want to be annoyed by "are you sure you want to delete?"
(setq ibuffer-expert t)

;; I want you to be up-to-date every time!
(add-hook 'ibuffer-mode-hook
	  '(lambda ()
	     (ibuffer-auto-mode 1)))


;; I really like it!
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; I want to save my minibuffer history from one session to another.
(savehist-mode 1)

;; I try company, but because it uses the same information that semantics,
;; I don't see why keep it.

;; Trying an undo visiualisation system:
;; Seems impressive. Keep it!
(load-file "~/.emacs.d/plugins/undo-tree.el")

;; When I use calendar, I want week starting the monday!
(setq calendar-week-start-day 1)

;; Insert function prototype in current header file and matching
;; function body in implementation file.
(defun enki-insert-new-method (rettype proto)
  "Insert a function declaration into the current class header file at
point, along with matching function definition in the corresponding
implementation file, complete with class name and scope resolution
operator. This function expects the implementation file to be named
foo.cc and in the same directory as the current header file, foo.hh."
  (interactive "sReturn type:\nsPrototype: ")
  (let ((classname (get-class-name))
        (c-tab-always-indent t))
    (if (null classname) (message "Not in class definition!")
      (unless (string-equal rettype "") (setq rettype (concat rettype " ")))
      (insert rettype proto ";")
      (c-indent-command)
      (save-window-excursion
        (c-switch-hh-cc)
        (end-of-buffer)
        (insert "\n\n")
        (end-of-buffer)
        (insert rettype classname "::" proto "\n{\n}\n")))))


(defun get-class-name ()
  "Allow to get the first backward line `class foo'. It returns foo."
  (save-excursion
    (re-search-backward "class \\(\\w+\\)")
    (buffer-substring (match-beginning 1) (match-end 1))))

;; Bind the enki-insert-new-method:
(define-key  c-mode-base-map  [(control c) (m)] 'enki-insert-new-method)

;; I want to copy from the cursor to the end of line without cuting.
;; Same thing from the cursor to the beginning.
(defun copy-to-end ()
  "Copy from point to end."
  (save-excursion
    (kill-ring-save (point) (line-end-position))))

(defun copy-to-begin ()
  "Copy from begin to point."
  (save-excursion
    (kill-ring-save (point) (line-beginning-position))))

(defun enki-kill-ring-save (&optional r)
  "If no argument is passed, copy from point to end. Otherwise copy
   from begin to end."
  (interactive "P")
  (if r
      (copy-to-begin)
    (copy-to-end)))

(global-set-key (kbd "C-M-k") 'enki-kill-ring-save)


;; It is really sad, when you work with camelCase notation
;; to have your emacs which doesn't understand the begining and the end of
;; subword. For example, initially ctrl + left when the cursor is at the
;; end of the word `thisIsMyWonderfulVariableName' put the pointer
;; just before `this'. I previously tried c-subword-mode which works at
;; home, but which is not defined at work. Moreover, if I want to copy a
;; word, which is written in camelCase, I must copy several word for one:
;; `this' `Is' `My' and so on.
;; So I must found another solution. Here it is! I rebind control left and
;; right to understand camelCase. BUT when I do M-@ before
;; `thisIsMyWonderfulVariableName' all the word is selected.
(add-hook 'find-file-hooks
	    (lambda ()
              (when (memq major-mode '(c-mode c++-mode))
                (global-set-key (kbd "C-<left>")
                                'c-backward-into-nomenclature)
                (global-set-key (kbd "C-<right>")
                                'c-forward-into-nomenclature))))

;; I found a beautiful thing in the mefyl's part of this conf... So I bind it.
;; Now, f9 will save your window configuration and which buffer is where,
;; and push it on a stack. f10 will simply pop it from the stack.
(global-set-key [f9] 'push-winconf)
(global-set-key [f10] 'pop-winconf)

;; perl variable style:
;; (I keep all old value, I change only the indentation when opening braces.
(setq perl-indent-level 2)
(setq cperl-indent-level 2)

;; (add-hook 'c-mode-hook        'flyspell-prog-mode 1)
;; (add-hook 'c++-mode-hook      'flyspell-prog-mode 1)
;; (add-hook 'cperl-mode-hook    'flyspell-prog-mode 1)
;; (add-hook 'makefile-mode-hook 'flyspell-prog-mode 1)
;; (add-hook 'python-mode-hook   'flyspell-prog-mode 1)
;; (add-hook 'sh-mode-hook       'flyspell-prog-mode 1)
;; (add-hook 'org-mode-hook      'flyspell-mode 1)


;; Useful to have several command in the background at the same time.
(defadvice erase-buffer (around erase-buffer-noop)
  "make erase-buffer do nothing")

(defadvice shell-command (around shell-command-unique-buffer activate compile)
  (if (or current-prefix-arg
          (not (string-match "[ \t]*&[ \t]*\\'" command)) ;; background
          (bufferp output-buffer)
          (stringp output-buffer))
      ad-do-it ;; no behavior change

    ;; else we need to set up buffer
    (let* ((command-buffer-name
            (format "*background: %s*"
                    (substring command 0 (match-beginning 0))))
           (command-buffer (get-buffer command-buffer-name)))

      (when command-buffer
        ;; if the buffer exists, reuse it, or rename it if it's still in use
        (cond ((get-buffer-process command-buffer)
               (set-buffer command-buffer)
               (rename-uniquely))
              ('t
               (kill-buffer command-buffer))))
      (setq output-buffer command-buffer-name)

      ;; insert command at top of buffer
      (switch-to-buffer-other-window output-buffer)
      (insert "Running command: " command
              "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n")

      ;; temporarily blow away erase-buffer while doing it, to avoid
      ;; erasing the above
      (ad-activate-regexp "erase-buffer-noop")
      ad-do-it
      (ad-deactivate-regexp "erase-buffer-noop"))))

;; Problem when you want to include a file, you can make several
;; typographicals errors. So I let emacs complete the filename.
;; Another problem is I don't want to say to him the include path
;; each time. So each time I want a file I say the directory, and the file.
(defun enki-insert-file-name ()
  (interactive); "*fSelect your top directory: \nP")
  (let ((dir (read-directory-name "Directory is: "))
        (file (read-file-name "File is: ")))
    (insert (replace-regexp-in-string dir "" file))))

(global-set-key "\C-ci" 'enki-insert-file-name)

;; FIXME ing1 2014:
;; A nice reading about emacs regular expression
;; is http://www.emacswiki.org/emacs/RegularExpression
(defun norme() (interactive)
  (highlight-regexp "if(" 'hi-red)
  (highlight-regexp "while(" 'hi-red)
  (highlight-regexp "for(" 'hi-red)
  (highlight-regexp ",[^ \t\n]" 'hi-red))

(add-hook 'find-file-hooks 'norme)

(add-to-list 'load-path
	     "~/.emacs.d/git/yasnippet")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/global-mode 1)

;; Develop in ~/emacs.d/mysnippets.
(setq yas/root-directory "~/.emacs.d/git/mysnippets")

;; Load the snippets.
(yas/load-directory yas/root-directory)


(message "ALL IS OKAY")


(my-load-file "~/.emacs.d/git/clang-complete/src/clang-completion-mode.el")

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(add-hook 'rst-mode-hook
          'flyspell-mode 1)

;; (add-to-list 'load-path "~/auctex-11.86")

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(setq TeX-auto-save t)
(setq TeX-parse-self t)

(setq-default TeX-master nil)

;; A little tuto:
;; C-c C-e: Insert environment
;; C-c C-s: Insert heading
;; C-c <RET>: Insert macro.
;; C-c C-j in itemize: Insert new item.
;; C-c C-r: Compile region
;; C-c C-f C-b: Bold region
;; C-c C-f C-i: Italic region
;; C-c C-f C-e: Emphasis region.
;; C-c ~: LaTeX-math-mode: ``' after backquote, expand. for example
;;                         ` i => \in.
;; C-c ;: comment a region
;; C-c %: Comment a paragraph.
