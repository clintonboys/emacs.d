(require 'find-lisp)

(defun clinton/publish (file)
  (with-current-buffer (find-file-noselect file)
    (let ((org-id-extra-files (find-lisp-find-files "/Users/clinton/roam/" "\.org")))
      (org-roam-to-hugo-md file))))
