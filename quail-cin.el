;;; quail-cin.el --- add new quail input method support from cin table

;; Copyright (C) 2013  Meng-Cheng Cheng (letoh)
;;


;;; Usage:
;;
;;   (quail-cin-load-file "greek.cin")
;;   (quail-cin-load-file "symbols.cin")
;;   (quail-cin-load-file "cj.cin")
;;
;; to load the cin file which is designed for phrase definitions:
;;
;;   (quail-cin-load-file "NewCJ3.cin" t)
;;

;;; Code:

;; Custom Variables:

(defvar cin-ime-name-prefix "quail-cin-")

(defvar cin-ime-language-environment "Chinese-BIG5")


(defun cin-attrs-get-cname (attrs &optional default)
  (or (cdr (assoc '%prompt attrs))
      (cdr (assoc '%cname attrs))
      default
      (cdr (assoc '%ename attrs))
      "CIN"))

(defun cin-attrs-get-pkgname (attrs &optional default)
  (let ((pkgname (cdr (assoc '%%pkgname attrs))))
    (concat cin-ime-name-prefix
	    (or pkgname default
		(cdr (assoc '%ename attrs))))))

(defun cin-attrs-get-prompt (attrs &optional default)
  (or (cdr (assoc '%%prompt attrs))
      default
      (cdr (assoc '%prompt attrs))
      (cdr (assoc '%cname attrs))
      "CIN"))

(defun cin-attrs-to-header (attrs)
  (let* ((ename (cdr (assoc '%ename attrs)))
	 (cname (cin-attrs-get-cname attrs ename))
	 (pkgname (cin-attrs-get-pkgname attrs ename)))
    (format (concat
	     "(require 'quail)\n\n"
	     "(quail-define-package \"%s\" \"%s\" \"[%s]\"\n"
	     " '(")
	    pkgname cin-ime-language-environment (cin-attrs-get-prompt attrs))))

(defun cin-attrs-to-footer (attrs)
  (let* ((ename (cdr (assoc '%ename attrs)))
	 (cname (cin-attrs-get-cname attrs ename)))
    (format (concat
	     "  ) \"Input Method Module %s for quail\n\n"
	     "This file is converted by quail-cin package.\n\n"
	     "\\\\\\\\<quail-translation-docstring>\n"
	     "\"\n"
	     " '((\"\\\\C-?\" . quail-delete-last-char)\n"
	     "   (\">\" . quail-next-translation)\n"
	     "   (\"<\" . quail-prev-translation)\n"
	     "   (\" \" . quail-select-current))\n"
	     " nil nil nil t)\n")
	    cname)))

(defun cin-safe-quote-key (key)
  (replace-regexp-in-string
   "\\\\" (regexp-quote (regexp-quote "\\\\"))
   key)
  )

(defun cin-safe-quote (val)
  (replace-regexp-in-string
   ";" (regexp-quote (regexp-quote "\\;"))
   (replace-regexp-in-string
    "\\\\" (regexp-quote (regexp-quote "\\\\"))
    (replace-regexp-in-string
     "\"" "#-#\\\""
      val t t)))
  )

(defun cin-filename-p (file-name &optional allow-dir)
  "Return t if the FILE-NAME is a valid cin file or a directory if ALLOW-DIR is t."
  (and (file-exists-p file-name)
       (or (and (eq 't allow-dir)
		(car (file-attributes file-name))
		(string= (file-name-nondirectory
			  (file-name-as-directory file-name)) ""))
	   (string= (file-name-extension file-name) "cin"))
       t))

(defun cin-parse-file (cin-file-name &optional phrase pkg-name prompt action)
  (when (cin-filename-p cin-file-name)
    (with-temp-buffer
      (insert-file-contents cin-file-name)
      (goto-char (point-min))
      ;; preprocessing
      (save-excursion
	;; some cin files lack of final end tag
	(when (and (re-search-forward "%chardef[ \t]*begin" nil "noerror")
		   (not (re-search-forward "%chardef[ \t]*end" nil "noerror")))
	  (goto-char (point-max))
	  (insert "%chardef end\n")))
      (save-excursion (delete-trailing-whitespace))
      (when (string= "" pkg-name) (setq pkg-name nil))
      (when (string= "" prompt) (setq prompt nil))
      ;;
      (let ((templ (if phrase "  (\"%s\" [\"%s\"])" "  (\"%s\" \"%s\")"))
	    (attrs (list (cons (intern "%%pkgname") pkg-name)
			 (cons (intern "%%prompt") prompt)))
	    section)
	(message "parsing %s ..." cin-file-name)
	(while (re-search-forward
		"[ \t]*\\([^ \n\t]+\\)[ \t]*\\([^\t\n]+\\)?" nil "noerror")
	  (let ((key   (match-string 1))
		(val   (match-string 2)))
	    (when (string-match-p "^%[^%]" key)
	      (cond ((string= val "begin") (progn
					     (setq section (intern key))
					     (message "converting %s ..." key)))))
	    (cond
	     ((eq section '%keyname)
	      (cond ((string= val "begin")
		     (replace-match (cin-attrs-to-header attrs)))
		    ((string= val "end")
		     (replace-match (cin-attrs-to-footer attrs)))
		    (t
		     (replace-match (format "  (%d . \"%s\")" (get-byte 0 key) val)))))
	     ((eq section '%chardef)
	      (cond ((string= val "begin")
		     (replace-match "(quail-define-rules"))
		    ((string= val "end")
		     (replace-match ")\n"))
		    (t
		     (replace-match (format templ (cin-safe-quote-key key)
					    (cin-safe-quote val))))))
	     (t
	      (progn
		(add-to-list 'attrs (cons (intern key) val))
		(replace-match "")))
	     )

	    (when (string-match-p "^%[^%]" key)
	      (cond ((string= val "end") (progn
					   (setq section nil)
					   (message "parsing %s ..." cin-file-name)))))
	    ))
	;; postprocess
	(goto-char (point-min))
	(replace-string "#-#\\" "")
	(goto-char (point-max))
	(insert (format "(provide '%s)\n\n" (cin-attrs-get-pkgname attrs)))
	(message "parsing %s finished, quail package name is `%s'"
		 cin-file-name (cin-attrs-get-pkgname attrs))
	;; action on the final result
	(when action
	  (funcall action))
	))))

(defun read-cin-attrs-from-minibuf (&optional buffer)
  (let* ((buf-file-name  (buffer-file-name))
	 (init-file-name (if (and buffer (cin-filename-p buf-file-name))
			     buf-file-name nil))
	 (cin-file-name (read-file-name
			 "cin file name: " nil nil t init-file-name
			 #'(lambda (file-name) (cin-filename-p file-name t)))))
    (list cin-file-name
	  (yes-or-no-p "Phrase based? ")
	  (read-string "IME name (leave empty to get from file): "
		       (file-name-sans-extension
			(file-name-nondirectory cin-file-name)))
	  (read-string "Prompt String (leave empty to get from file): "))))

(defun quail-cin-convert-to-quail (cin-file-name &optional phrase pkg-name prompt)
  "Convert a cin file to quail package and save the result to a file."
  (interactive (read-cin-attrs-from-minibuf t))
  (cin-parse-file cin-file-name phrase pkg-name prompt #'save-buffer))

(defun quail-cin-load-file (cin-file-name &optional phrase pkg-name prompt)
  "Load a cin file as a quail package."
  (interactive (read-cin-attrs-from-minibuf))
  (cin-parse-file cin-file-name phrase pkg-name prompt #'eval-buffer))


;; End:

(provide 'quail-cin)

;;; quail-cin.el ends here
