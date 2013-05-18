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


(defun cin-attrs-get-cname (attrs &optional default)
  (or (cdr (assoc '%prompt attrs))
      (cdr (assoc '%cname attrs))
      default
      (cdr (assoc '%ename attrs))
      "CIN"))

(defun cin-attrs-to-header (attrs)
  (let* ((ename (cdr (assoc '%ename attrs)))
	 (cname (cin-attrs-get-cname attrs ename))
	 (prefix cin-ime-name-prefix))
    (format (concat
	     "(require 'quail)\n\n"
	     "(quail-define-package \"%s%s\" \"Chinese-BIG5\" \"[%s]\"\n"
	     " '(")
	    prefix ename cname)))

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
	     "   (\"<\" . quail-prev-translation))\n"
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

(defun cin-parse-file (cin-file-name &optional phrase action)
  (when (file-exists-p cin-file-name)
    (with-temp-buffer
      (insert-file-contents cin-file-name)
      ;; some cin files lack of final end tag
      (when (and (re-search-forward "%chardef[ \t]*begin" nil "noerror")
		 (not (re-search-forward "%chardef[ \t]*end" nil "noerror")))
	(goto-char (point-max))
	(insert "%chardef end\n"))
      ;;
      (goto-char (point-min))
      (save-excursion (delete-trailing-whitespace))
      (let (section attrs)
	(while (re-search-forward
		"[ \t]*\\([^ \n\t]+\\)[ \t]*\\([^\t\n]+\\)?" nil "noerror")
	  (let ((key   (match-string 1))
		(val   (match-string 2))
		(templ (if phrase "  (\"%s\" [\"%s\"])" "  (\"%s\" \"%s\")")))
	    (when (string-match-p "^%[^%]" key)
	      (cond ((string= val "begin") (setq section (intern key)))))
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
	      (cond ((string= val "end")   (setq section nil))))
	    ))
	;; postprocess
	(goto-char (point-min))
	(replace-string "#-#\\" "")
	(goto-char (point-max))
	(insert (format "(provide '%s%s)\n\n"
			cin-ime-name-prefix (cdr (assoc '%ename attrs))))
	(when action
	  (funcall action))
	))))

(defun quail-cin-convert-to-quail (cin-file-name &optional phrase)
  (cin-parse-file cin-file-name phrase #'save-buffer))

(defun quail-cin-load-file (cin-file-name &optional phrase)
  (interactive (list
		(read-file-name
		 "cin file name: " nil nil t nil
		 #'(lambda (file-name)
		     (if (string= (file-name-nondirectory file-name) "")
			 t
		       (string= (file-name-extension file-name) "cin"))))
		(yes-or-no-p "phrase based? ")))
  (cin-parse-file cin-file-name phrase #'eval-buffer))


;; End:

(provide 'quail-cin)

;;; quail-cin.el ends here
