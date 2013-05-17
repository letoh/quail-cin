;;; quail-cin.el --- add new quail input method support from cin table

;; Copyright (C) 2013  Meng-Cheng Cheng (letoh)
;;


;;; Usage:
;;
;; (xy-quail-load-cin "greek.cin")
;; (xy-quail-load-cin "symbols.cin")
;; (xy-quail-load-cin "cj.cin")
;;

;;; Code:

(defun cin-attrs-to-header (attrs)
  (let* ((ename (cdr (assoc '%ename attrs)))
	 (cname (or (cdr (assoc '%prompt attrs))
		    ename)))
    (format (concat
	     "(require 'quail)\n"
	     "(quail-define-package \"lime-%s-quail\" \"Chinese\" \"[%s]\"\n"
	     " '(")
	    ename cname)))

(defun cin-attrs-to-footer (attrs)
  (format (concat
	   "  ) \"LIME %s for quail\n\n"
	   "\\\\\\\\<quail-translation-docstring>\n"
	   "\"\n"
	   " '((\"\\\\C-?\" . quail-delete-last-char)\n"
	   "   (\">\" . quail-next-translation)\n"
	   "   (\"<\" . quail-prev-translation))\n"
	   " nil nil nil t)")
	  (cdr (assoc '%prompt attrs))
	  ))

(defun cin-safe-quote (key)
  ;;(replace-regexp-in-string "\\\\" "\\\\\\\\" key) val)))))
  (replace-regexp-in-string
   ";" (regexp-quote (regexp-quote "\\;")) ;"\\\\\\\\;"
   (replace-regexp-in-string
    "\\\\" (regexp-quote (regexp-quote "\\\\")) ;"\\\\\\\\\\\\\\\\"
    key))
  )

(defun parse-cin (cin-file-name &optional action)
  (when (file-exists-p cin-file-name)
    (with-temp-buffer
    ;(with-output-to-temp-buffer "*cin*"
    ;(with-temp-file "/home/letoh/src/elisp/aaa"
      (insert-file-contents cin-file-name)
      ;;
      (when (and (re-search-forward "%chardef[ \t]*begin" nil "noerror")
		 (not (re-search-forward "%chardef[ \t]*end" nil "noerror")))
	(goto-char (point-max))
	(insert "%chardef end\n"))
      ;;
      (goto-char 1)
      (let (section attrs)
	(while (re-search-forward
		"[ \t]*\\([^ \n\t]+\\)[ \t]*\\([^ \t\n]+\\)?" nil "noerror")
	  (let ((key (match-string 1))
		(val (match-string 2)))
	    ;;(save-match-data
	      (when (string-match-p "^%[^%]" key)
		(cond ((string= val "begin") (setq section (intern key)))))
	      ;;)
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
		     (replace-match (format
				     "  (\"%s\" \"%s\")"
				     (cin-safe-quote key) val)))))
	     (t
	      (progn
		(add-to-list 'attrs (cons (intern key) val))
		(replace-match "")))
	     )
	    ;;(princ (format "'%s' : '%s'\n" key val))
	    ;;(save-match-data
	      (when (string-match-p "^%[^%]" key)
		(cond ((string= val "end")   (setq section nil))))
	      ;;)
	    ))
	;;(print attrs)
	(goto-char (point-max))
	(insert (format "(provide 'lime-%s-quail)\n\n" (cdr (assoc '%ename attrs))))
	;;(save-buffer)
	(when action
	  (funcall action))
	))))

(defun xy-quail-convert-cin-to-quail (cin-file-name)
  (parse-cin cin-file-name #'save-buffer))

(defun xy-quail-load-cin (cin-file-name)
  (interactive)
  (parse-cin cin-file-name #'eval-buffer))


;; End:

(provide 'quail-cin)

;;; quail-cin.el ends here
