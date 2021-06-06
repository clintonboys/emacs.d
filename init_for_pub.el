(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("elpa"  . "https://elpa.gnu.org/packages/")
			 ("org"   . "http://orgmode.org/elpa/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(require 'org-roam)
(setq org-confirm-babel-evaluate nil)
(use-package org-roam
        :hook 
        (after-init . org-roam-mode)
        :custom
        (org-roam-directory "/Users/clinton/roam")
        :bind (:map org-roam-mode-map
                (("C-c n l" . org-roam)
                 ("C-c n f" . org-roam-find-file)
                 ("C-c n b" . org-roam-switch-to-buffer)
                 ("C-c n g" . org-roam-show-graph)
                 ("C-c n r" . org-roam-db-build-cache))
                :map org-mode-map
                (("C-c i" . org-roam-insert)))
        :config
(defun org-roam--title-to-slug (title)
  "Convert TITLE to a filename-suitable slug. Uses hyphens rather than underscores."
  (cl-flet* ((nonspacing-mark-p (char)
                                (eq 'Mn (get-char-code-property char 'general-category)))
             (strip-nonspacing-marks (s)
                                     (apply #'string (seq-remove #'nonspacing-mark-p
                                                                 (ucs-normalize-NFD-string s))))
             (cl-replace (title pair)
                         (replace-regexp-in-string (car pair) (cdr pair) title)))
    (let* ((pairs `(("[^[:alnum:][:digit:]+]" . "-")  ;; convert anything not alphanumeric or plus <---
                    ("--*" . "-")  ;; remove sequential underscores
                    ("^-" . "")  ;; remove starting underscore
                    ("-$" . "")))  ;; remove ending underscore
           (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
      (s-downcase slug)))))

(setq org-roam-db-location "/Users/clinton/org-roam.db")

 (defun clinton/deft-setup ()
    (visual-line-mode 0))


(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (shell .t)
    (python . t)))

(defun clinton/markdown-mode-setup ()
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package markdown-mode
  :ensure t
  :hook (markdown-mode . clinton/markdown-mode-setup)
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  )

(use-package magit)

(use-package ox-hugo
  :ensure t 
  :after ox)

(setq org-hugo-default-section-directory "/Users/clinton/Documents/technical/dev/digital-garden")

(defun title-capitalization (str)
  "Convert str to title case"
  (interactive)
  (with-temp-buffer
    (insert str)
    (let* ((beg (point-min))
           (end (point-max))
	   ;; Basic list of words which don't get capitalized according to simplified rules
	   ;; http://karl-voit.at/2015/05/25/elisp-title-capitalization/
           (do-not-capitalize-basic-words '("a" "ago" "an" "and" "as" "at" "but" "by" "for"
                                            "from" "in" "into" "it" "next" "nor" "of" "off"
                                            "on" "onto" "or" "over" "past" "so" "the" "till"
                                            "to" "up" "yet"
                                            "n" "t" "es" "s"))
	   ;; If user has defined 'my-do-not-capitalize-words, append to basic list
           (do-not-capitalize-words (if (boundp 'my-do-not-capitalize-words)
                                        (append do-not-capitalize-basic-words my-do-not-capitalize-words )
                                      do-not-capitalize-basic-words)))
      ;; Go to begin of car word
      (goto-char beg)
      (setq continue t)

      ;; Go through the region, word by word
      (while continue
        (let ((last-point (point)))
          (let ((word (thing-at-point 'word)))
            (if (stringp word)
                ;; Capitalize current word except when it is list member
                (if (and (member (downcase word) do-not-capitalize-words)
                         ;; Always capitalize car word
                         (not (= (point) 1)))
                    (downcase-word 1)

                  ;; If it's an acronym, don't capitalize
                  (if (string= word (upcase word))
                      (progn
                        (goto-char (+ (point) (length word) 1)))
                    (capitalize-word 1)))))

          (skip-syntax-forward "^w" end)

          ;; Break if we are at the end of the buffer
          (when (= (point) last-point)
            (setq continue nil))))

      ;; Always capitalize the last word
      (backward-word 1)

      (let ((word (thing-at-point 'word)))
        (if (and (>= (point) 0)
                 (not (member (or word "s")
                              '("n" "t" "es" "s")))
                 (not (string= word (upcase word))))
            (capitalize-word 1))))

    (buffer-string)))

(defun clinton/org-roam--backlinks-list (file)
          (if (org-roam--org-roam-file-p file)
              (--reduce-from
               (concat acc (format "- [[file:%s][%s]]\n"
			       (file-relative-name (car it) "/Users/clinton/roam")
			       (title-capitalization
			 	  (replace-regexp-in-string "-" " "
				     (replace-regexp-in-string "[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]-" ""
							       (replace-regexp-in-string "-org" ""
						(org-roam--title-to-slug (file-relative-name (car it) "/Users/clinton/roam"))))))
	  ))
               ""
               (org-roam-db-query

          [:select :distinct [links:source]
                   :from links
                   :left :outer :join tags :on (= links:source tags:file)
                   :where (and (= dest $s1) (like tags:tags '%public%))]
                file))
            ""))


  (defun clinton/org-roam--extract-note-body (file)
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (car (org-element-map (org-element-parse-buffer) 'paragraph
               (lambda (paragraph)
                 (let ((begin (plist-get (car (cdr paragraph)) :begin))
                       (end (plist-get (car (cdr paragraph)) :end)))
                   (buffer-substring begin end)))))))

;; Fetches all org-roam files and exports to hugo markdown
;; files. Adds in necessary hugo properties
;; e.g. HUGO_BASE_DIR. Only exports files marked as public. 
(setq org-roam-publish-path "~/dev/digital-garden")
(defun file-path-to-slug (path)
  (let* ((file-name (car (last (split-string path "--"))))
         (title (car (split-string file-name "\\."))))
    (replace-regexp-in-string (regexp-quote "_") "-" title nil 'literal)))
(defun file-path-to-md-file-name (path)
  (let ((file-name (car (last (split-string path "/")))))
    (concat (car (split-string file-name "\\.")) ".md")))
(defun org-roam-to-hugo-md (f)
  ;;(interactive)
  ;; Make sure the author is set
  (setq user-full-name "Clinton Boys")
  (setq org-hugo-base-dir "~/dev/digital-garden")
  ;(let ((files (mapcan
  ;              (lambda (x) x)
   ;             (org-roam-db-query
    ;            [:select :distinct [files:file]
     ;            :from files
      ;           :left :outer :join tags :on (= files:file tags:file)
       ;          :where (like tags:tags '%public%)]))));
         (message "Working on this: %s" f)

         (goto-char (point-min))
         (insert
          (format "#+HUGO_BASE_DIR: %s\n#+HUGO_SECTION: ./\n#+HUGO_SLUG: %s\n#+EXPORT_FILE_NAME: %s\n#+EXPORT_HUGO_CUSTOM_FRONT_MATTER: :summary %s\n"
                  org-roam-publish-path
                  (file-path-to-slug (file-relative-name f "/Users/clinton/roam"))
                  (file-path-to-md-file-name f)
		  (quote "\"This is a page on Mt. Solitary\"")))
         (if (eq (clinton/org-roam--extract-note-body f) nil)
             (progn
               (goto-char (point-max))
               (insert "\n/This note does not have a description yet./\n")))
         (org-hugo-export-to-md)
     )
