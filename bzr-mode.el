;;; bzr-mode.el --- Bzr mode for (X)Emacs.

;;        Licensed under the GNU General Public License.

;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2 of the License, or
;;    (at your option) any later version.

;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;    GNU General Public License for more details.

;;; Commentary:

;; A major mode for writing bzr files
;; Provides: syntax highlighting and indentation


;; TODO
;; 1.) Electric, abbrv, auto-completion
;; 2.) Looking-back needs to be added
;; 3.) Fix complains

;; To enable put this into your .emacs files
;; (require 'bzr-mode)
;; (setq auto-mode-alist (cons '("\\.ept$" . bzr-mode) auto-mode-alist))

;;; Code:

(require 'generic-x)

;; Define the indentation model
(defun bzr-indent-line ()
  "Indent current line as bzr code"
  (interactive)
  (if (bobp)
      (indent-line-to 0)
    ;; else you do this single statement
    (let ((not-indented t) 
	  (cur-indent 0)
	  (end-count 0)
	  (automaton-count 0)
	  (tel-count 0)
	  (let-count 0)
	  )
      (if (posix-looking-at "^[ \t]*\\(end\\)")
	  (save-excursion ;; save the current context of the point
	    (while not-indented
	      (forward-line -1) ;; go to previous line
	      (if (posix-looking-at "^[ \t]*\\(automaton\\)")
		  (if (= automaton-count end-count)
		      (progn
			(setq not-indented nil)
			(setq cur-indent (current-indentation))
			)
		    (setq automaton-count (+ automaton-count 1)))
	  	;; taking care of infinite loops, it might reach the top
	  	;; searching for something to match
	  	(if (bobp)
	  	    (progn
	  	      ;; then just break out
	  	      (setq cur-indent (current-indentation))
	  	      (setq not-indented nil))
	  	  (if (looking-at "^[ \t]*\\(end\\)")
		      (setq end-count (+ end-count 1))
		    )))))
	(if(posix-looking-at "^[ \t]*\\(tel\\)")
	    (save-excursion ;; save the current context of the point
	      (while not-indented
		(forward-line -1) ;; go to previous line
		(if (posix-looking-at "^[ \t]*\\(let\\)")
		    (if (= let-count tel-count)
			(progn
			  (setq not-indented nil)
			  (setq cur-indent (current-indentation)))
		      (setq let-count (+ let-count 1)))
		  ;;taking care of recursive error
		  (if (bobp)
		      (progn
			;; then just break out
			(setq cur-indent (current-indentation))
			(setq not-indented nil))
		    (if (posix-looking-at "^[ \t]*\\(tel\\)")
			(setq tel-count (+ tel-count 1)))))))
	  (if(posix-looking-at "^[ \t]*\\(do\\)")
	      (save-excursion ;; save the current context of the point
		(while not-indented
		  (forward-line -1) ;; go to previous line
		  (if (posix-looking-at "^[ \t]*\\(state\\)")
		      (progn
			(setq not-indented nil)
			(setq cur-indent (current-indentation))
			)
		    ;;taking care of recursive error
		    (if (bobp)
			(progn
			  ;; then just break out
			  (setq cur-indent (current-indentation))
			  (setq not-indented nil))))))
	    ;; else-if we have the state keyword then
	    (if (posix-looking-at "^[ \t]*\\(state\\)")
		;; if the next word is state then
		(save-excursion
		  (while not-indented
		    (forward-line -1)
		    ;; if you get the word do
		    (if (posix-looking-at "^[ \t]*\\(do\\)")
			;; then reduce the indentation
			(progn
			  (setq cur-indent (current-indentation))
			  (setq not-indented nil)
			  )
		      ;;else if you get the word automaton
		      (if (posix-looking-at "^[ \t]*\\(automaton\\)")
			  (progn
			    (setq cur-indent (+ (current-indentation) tab-width))
			    (setq not-indented nil))
			;; else you get the start of line
			(if (bobp)
			    (progn
			      ;; then just break out
			      (setq cur-indent (current-indentation))
			      (setq not-indented nil)))))))
	      (progn
		(if (< cur-indent 0)
		    (setq cur-indent 0))
		(save-excursion 
		  ;; (print "not-indented is")
		  ;; (print not-indented)
		  ;; (print "bobp is")
		  ;; (print (bobp))
		  (while not-indented
		    (forward-line -1)
		    (if (posix-looking-at "^[ \t]*\\(tel\\|end\\)")
			(progn
			  ;; (print "found tel|end atop")
			  (setq cur-indent (current-indentation))
			  (setq not-indented nil))
		      (if (posix-looking-at "^[ \t]*\\(let\\|do\\|automaton\\)")
			  (progn
			    ;; (print "found let|automaton|state|do a top")
			    (setq cur-indent (+ (current-indentation) tab-width))
			    (setq not-indented nil))
			(if (bobp) ; Check for rule 5
			    (setq not-indented nil))
			)))))))))
      (if cur-indent
	  (indent-line-to cur-indent)
	(indent-line-to 0)))))


(define-generic-mode 'bzr-mode
  '("(*")
  '("let" "tel" "automaton" "state" "do" "end" "node" "contract" "until" "then" "true" "false" "returns" "not" "&"
    "|" "or" "int" "bool" "var" "assume" "enforce" "inlined")
  '(("=" . 'font-lock-operator)
    ("+" . 'font-lock-operator)
    ("-" . 'font-lock-operator)
    )
  '("\\.ept$")
  (list 
   (lambda () (setq comment-start "(*"))
   (lambda () (setq comment-end "*)"))
   (lambda () (make-local-variable 'indent-line-function))
   (lambda () (setq tab-width 4))
   (lambda () (setq indent-line-function 'bzr-indent-line))
   (lambda ()(defvar bzr-mode-map
	       (let ((map (make-keymap)))
		 (define-key map "\C-j" 'newline-and-indent)
		 map)
	       "Keymap for bzr major mode")) 
   (lambda ()(defvar bzr-mode-hook nil))
   ;; FIXME 
   ;; when one writes the "state" keyword, it does not go back on its
   ;; own
   (lambda () (add-hook 'bzr-mode-hook '(lambda ()
					  (local-set-key (kbd "RET") 'newline-and-indent))))
   ))

(provide 'bzr-mode)
