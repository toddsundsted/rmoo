;;; Prefix/suffix regions with strings
;;;
;;; Original Author: Ron Tapia <tapia@nmia.com>
;;; $Author: mattcamp $
;;; $Date: 1999/03/01 23:04:22 $
;;; RRevision$

(defun surround-region (before prefix suffix after &optional start end)
  "Place specified strings before, PREFIXing each line, SUFFIXing
each line, and AFTER the specified region.  When called interactively,
acts on the current region."
  (interactive "sBefore: \nsPrefix: \nsSuffix: \nsAfter: \nr")
  (save-excursion
    (narrow-to-region (or start (region-beginning)) (or end (region-end)))
    (goto-char (point-min))
    (untabify (point-min) (point-max))
    (if (not (equal before "")) (insert before "\n"))
    (while (re-search-forward "^\\(.*\\)$" (1- (point-max)) "end")
      (replace-match (concat prefix "\\1" suffix) t))
    (forward-char 1)
    (insert after)
    (widen)))

(defun prefix-region-with-string (prefix &optional suffix)
  "Get string from user, and prefix each line of the current region with it.
If current-prefix-arg (\\[universal-argument]), prompt for string to suffix to region as well."
  (interactive (list (read-string "Prefix: ")
		     (if current-prefix-arg
			 (read-string "Suffix: ")
		       "")))
  (surround-region "" prefix suffix ""))

(defun prefix-region-with-quote ()
  "Prefix current region with the quote character (\")."
  (interactive)
  (prefix-region-with-string "\""))

(defun prefix-region-with-pose ()
  "Prefix current region with the pose character (:)."
  (interactive)
  (prefix-region-with-string ":"))

(defun prefix-region-with-> ()
  "Prefix current region with the > character."
  (interactive)
  (prefix-region-with-string ">"))

;;; $Log: prefix.el,v $
;;; Revision 1.1  1999/03/01 23:04:22  mattcamp
;;; Initial revision
;;;
