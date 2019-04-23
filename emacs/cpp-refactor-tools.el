;;; -*- lexical-binding: t -*-

(defun t:search-forward-c++-namespace (&optional bound)
  (let ((orig-point (point))
        (ns-regex (rx (and bol "namespace" (1+ space)
                           (group (1+ (or (syntax word) (syntax symbol) ":")))
                           (0+ space)
                           "{"
                           (0+ space)
                           eol))))
    (search-forward-regexp ns-regex bound t)
    (xwhen (/= (point) orig-point)
      (match-string-no-properties 1))))

(defsubst t:next-line-end-position ()
  (line-end-position 2))

(defun t:flatten-c++-namespaces ()
  (interactive)
  (save-mark-and-excursion
    (beginning-of-buffer)
    (when-let ((topmost-ns (t:search-forward-c++-namespace)))
      (let ((match-beg (line-beginning-position))
            (namespaces (list topmost-ns))
            (match-overlay nil))
        ;; grovel consecutive namespaces
        (while (when-let ((ns (t:search-forward-c++-namespace (t:next-line-end-position))))
                 (push ns namespaces)
                 t))
        (let ((match-end (line-end-position))
              (replacement (concat "namespace "
                                   (string-join (nreverse namespaces) "::")
                                   " {")))
          (setq match-overlay (make-overlay match-beg match-end))
          (overlay-put match-overlay 'face 'query-replace)
          (let ((yes (y-or-n-p (format "Replace with `%s'? " replacement))))
            (delete-overlay match-overlay)
            (when yes
              (delete-region match-beg match-end)
              (insert replacement))))))))
