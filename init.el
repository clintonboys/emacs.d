(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)            ; Disable the toolbar
      (tooltip-mode -1)             ; Disable tooltips
      (menu-bar-mode -1)            ; Disable the menu bar
      (scroll-bar-mode -1)))

(setq custom-file (concat user-emacs-directory "/custom.el"))

(defvar clinton/personal-mac-name "Clintons-Air")
(defvar clinton/work-mac-name "Clinton-Boys-MacBook-Pro.local")

(setq mac-command-modifier 'alt
      mac-option-modifier 'meta
      mac-command-modifier 'hyper
      mac-right-option-modifier nil)

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
(setq use-package-always-ensure t)

;; (when (memq window-system '(mac ns x))
;;   (exec-path-from-shell-initialize))
(use-package exec-path-from-shell)
(when (memq window-system '(mac ns))
  (setenv "SHELL" "/bin/zsh")
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PYTHONPATH")))

(defvar clinton/default-font-size 120)
(defvar clinton/default-variable-font-size 120)

(setq inhibit-startup-message t)

;(scroll-bar-mode -1)          ; Disable visible scrollbar
(tool-bar-mode -1)            ; Disable the toolbar
(tooltip-mode -1)             ; Disable tooltips
(menu-bar-mode -1)            ; Disable the menu bar

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		    org-agenda-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "Fira Code" :height clinton/default-font-size :weight 'light)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height clinton/default-font-size :weight 'light)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Fira Code" :height clinton/default-variable-font-size)

;; Make ESC quit prompts
 (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

 (global-set-key (kbd "C-x /") 'comment-region)

 (global-unset-key (kbd "C-l"))
 (global-unset-key (kbd "C-x f"))

 ;;;*** which-key

 (use-package which-key
   :init
   (setq which-key-separator " ")
   (setq which-key-prefix-prefix "+")
   (which-key-mode)
   :diminish which-key-mode
   :config
   (setq which-key-idle-delay 1))

(use-package general
  :config
  (general-create-definer clinton/leader-c
    :prefix "C-c")
  (general-create-definer clinton/leader-l
    :prefix "C-l"))

  (clinton/leader-c
    "a" 'org-agenda
    "c" 'org-capture
    "d" 'deft
    "p" 'projectile-command-map
    "t" '(lambda () (interactive) (ansi-term "zsh"))
    "w" 'count-words
    "x" 'org-roam-to-hugo-md)

  (clinton/leader-l
    "a" 'avy-goto-char-2
    "c" 'helm-make-projectile
    "d" 'xref-find-definitions
    "j" 'json-pretty-print    
    "r" 'xref-find-references
    "t" '(counsel-load-theme :which-key "choose theme")
    "v" 'valign-table
    "x" 'org-babel-execute-src-block)

(use-package doom-themes
  :init (load-theme 'doom-one t))
(use-package all-the-icons)
(use-package dtrt-indent)

(setq dtrt-indent-original-indent 0)
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format "emacs")

(setq-default line-spacing 0.25)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         ("C-x b" . 'ivy-switch-buffer))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind ("C-x b" . 'counsel-switch-buffer)
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package helm
    :ensure t
    :init
    (setq helm-mode-fuzzy-match t)
    (setq helm-completion-in-region-fuzzy-match t)
    (setq helm-candidate-number-list 50))

  (use-package projectile)

;;  (add-hook 'after-init-hook 'global-company-mode)

(use-package deadgrep)
(define-key global-map [remap xref-find-references] 'deadgrep)

;;The helpful package improves emacs default help buffers.

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; (use-package epa-file
;;     :config
;;     (setq epa-file-encrypt-to '("clintonboys@icloud.com"))
;;     :custom
;;     (epa-file-select-keys 'silent))
;; (custom-set-variables '(epg-gpg-program  "/usr/local/MacGPG2/bin/gpg2"))
;; (epa-file-enable)

;; (defun clinton/org-font-setup ()
;;   ;; Replace list hyphen with dot
;;   (font-lock-add-keywords 'org-mode
;;                           '(("^ *\\([-]\\) "
;;                              (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;;   ;; Set faces for heading levels
;;   (dolist (face '((org-level-1 . 1.2)
;;                   (org-level-2 . 1.1)
;;                   (org-level-3 . 1.05)
;;                   (org-level-4 . 1.0)
;;                   (org-level-5 . 1.1)
;;                   (org-level-6 . 1.1)
;;                   (org-level-7 . 1.1)
;;                   (org-level-8 . 1.1)))
;;     (set-face-attribute (car face) nil :font "ETBembo" :weight 'regular :height (cdr face)))

;;   ;; Ensure that anything that should be fixed-pitch in Org files appears that way
;;   (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
;;   (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
;;   (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
;;   (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
;;   (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
;;   (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
;;   (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
;;   (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
;;   (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch))


 (custom-set-faces
 '(default     ((t (:foreground "#BBC2CF"))))
 '(org-level-1 ((t (:foreground "#BF9D7A"))))
 '(org-level-2 ((t (:foreground "#E4E9CD"))))
 '(org-level-3 ((t (:foreground "#EBF2EA"))))
 '(org-level-4 ((t (:foreground "#0ABDA0"))))
 '(org-level-5 ((t (:foreground "#80ADD7")))))

(defun clinton/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :pin org
  :hook (org-mode . clinton/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-agenda-start-on-weekday 0)

  (setq org-agenda-files
        '("~/Dropbox/org/inbox.org"
          "~/Dropbox/org/technical.org"
          "~/Dropbox/org/creative.org"
          "~/Dropbox/org/personal.org"
          "~/Dropbox/org/projects.org"
          "~/Dropbox/org/work.org"))

 (setq org-agenda-prefix-format
       '((agenda . " %i %?-12t% s")
         (todo   . " %i")
         (tags   . " %i %-12:c")
         (search . " %i %-12:c")))

 (require 'org-habit)
 (add-to-list 'org-modules 'org-habit)
 (setq org-habit-graph-column 60)

 (setq org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
     (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

 (setq org-refile-targets '((nil :maxlevel . 9)
                                (org-agenda-files :maxlevel . 9)))
 (setq org-refile-use-outline-path 'file)
 (setq org-outline-path-complete-in-steps nil)
 (advice-add 'org-refile :after 'org-save-all-org-buffers)

 (setq org-tag-alist
   '((:startgroup)
      ; Put mutually exclusive tags here
      (:endgroup)
      ("@errand" . ?E)
      ("@home" . ?H)
      ("@work" . ?W)
      ("agenda" . ?a)
      ("planning" . ?p)
      ("publish" . ?P)
      ("batch" . ?b)
      ("note" . ?n)
      ("idea" . ?i))))
 (setq org-agenda-hide-tags-regexp ".")
 (setq org-agenda-log-mode-items '(closed clock state))
 (setq org-habit-show-all-today t)

(setq org-agenda-custom-commands
      '(("a" "Personal"
     ((agenda ""
              ((org-agenda-span
                (quote day))
                     (org-agenda-files (quote ("/Users/clinton/Dropbox/org/personal.org"
					            "/Users/clinton/Dropbox/org/projects.org"
                                              "/Users/clinton/Dropbox/org/technical.org")))
               (org-deadline-warning-days 7)
               (org-agenda-overriding-header "Agenda\n")))
      (todo "TODO"
            ((org-agenda-overriding-header "To Refile\n")
                                  (org-agenda-prefix-format "  ")
             (org-agenda-files
              (quote
               ("/Users/clinton/Dropbox/org/inbox.org")))))
      (todo "NEXT"
            ((org-agenda-overriding-header "Projects\n")
                   (org-agenda-prefix-format "  %c (%e) | ")
             (org-agenda-files
              (quote
               ("/Users/clinton/Dropbox/org/projects.org")))))
                 ))

             ("w" "Work"
                  ((agenda ""
              ((org-agenda-span
                (quote day))
                     (org-agenda-files (quote ("/Users/clinton/Dropbox/org/work.org")))
               (org-deadline-warning-days 14)
               (org-agenda-overriding-header "Via\n")))
    (todo "TODO"
          ((org-agenda-overriding-header "To Refile\n")
           (org-agenda-files
            (quote
             ("/Users/clinton/Dropbox/org/work_inbox.org")))))
    (todo "NEXT"
          ((org-agenda-overriding-header "Projects\n")
           (org-agenda-prefix-format "  %i %-12:c [%e] ")
           (org-agenda-files
            (quote
             ("/Users/clinton/Dropbox/org/work.org")))))
  (todo "WAITING"
          ((org-agenda-overriding-header "Waiting on others\n")
           (org-agenda-files
            (quote
             ("/Users/clinton/Dropbox/org/work.org")))))
               ))))

    (setq org-capture-templates
            '(("w" "Work" entry (file+headline "/Users/clinton/Dropbox/org/work_inbox.org" "To refile")
                     "* TODO %?\n %i\n %a")
              ("p" "Personal" entry (file "/Users/clinton/Dropbox/org/inbox.org")
                   "* TODO %?\n %i\n %a")))
(setq org-agenda-window-setup "current-window")

(use-package org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  ("C-c y" . journal-file-yesterday)
  :custom
  (org-journal-date-prefix "#+title: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "/Users/clinton/Library/Mobile Documents/iCloud~is~workflow~my~workflows/Documents/org-roam/")
  (org-journal-date-format "%Y-%m-%d")
  :preface
  (defun get-journal-file-today ()
    "Gets filename for today's journal entry."
    (let ((daily-name (format-time-string "%Y-%m-%d.org")))
      (expand-file-name (concat org-journal-dir daily-name))))

  (defun journal-file-today ()
    "Creates and load a journal file based on today's date."
    (interactive)
    (find-file (get-journal-file-today)))

  (defun get-journal-file-yesterday ()
    "Gets filename for yesterday's journal entry."
    (let* ((yesterday (time-subtract (current-time) (days-to-time 1)))
           (daily-name (format-time-string "%Y-%m-%d.org" yesterday)))
      (expand-file-name (concat org-journal-dir daily-name))))

  (defun journal-file-yesterday ()
    "Creates and load a file based on yesterday's date."
    (interactive)
    (find-file (get-journal-file-yesterday)))
 )

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


 (use-package deft
   :hook (deft-mode . clinton/deft-setup)
   :init
   (setq deft-directory "~/roam")
   (setq-default truncate-lines t)
 )
 (require 'deft)

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (shell .t)
    (python . t)))

(defvar clinton/init-org-file (concat user-emacs-directory "init.org"))
(defvar clinton/init-el-file  (concat user-emacs-directory "init.el"))

(defun clinton/tangle-on-save ()
  (when (equal (buffer-file-name)
               (expand-file-name clinton/init-org-file))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle)
      (message "init.el tangled from init.org"))))

(add-hook 'after-save-hook 'clinton/tangle-on-save)

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

(defun clinton/lsp-mode-setup ()
  (setq xref-prompt-for-identifier '(not xref-find-definitions
                                        xref-find-definitions-other-window
                                        xref-find-definitions-other-frame
                                        xref-find-references)))

(use-package lsp-mode
  :defer t
  :commands lsp
  :custom
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
  (lsp-file-watch-threshold 2000)
  (read-process-output-max (* 1024 1024))
  (lsp-eldoc-hook nil)
  :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
  :hook ((java-mode python-mode go-mode
          js-mode js2-mode typescript-mode web-mode
          c-mode c++-mode objc-mode) . lsp))

(use-package lsp-ui
  :after lsp-mode
  :diminish
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references)
        ("C-c u" . lsp-ui-imenu)
        ("M-i" . lsp-ui-doc-focus-frame))
  (:map lsp-mode-map
        ("M-n" . forward-paragraph)
        ("M-p" . backward-paragraph))
  :custom
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  :config
  ;; Use lsp-ui-doc-webkit only in GUI
  (if (display-graphic-p)
      (setq lsp-ui-doc-use-webkit t))
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))

  (use-package flycheck
    :defer t
    :hook (lsp-mode . flycheck-mode))

 (use-package company
   :diminish company-mode
   :hook ((prog-mode LaTeX-mode latex-mode ess-r-mode) . company-mode)
   :bind
   (:map company-active-map
         ([tab] . smarter-tab-to-complete)
         ("TAB" . smarter-tab-to-complete))
   :custom
   (company-minimum-prefix-length 1)
   (company-tooltip-align-annotations t)
   (company-require-match 'never)
   ;; Don't use company in the following modes
   (company-global-modes '(not shell-mode eaf-mode))
   ;; Trigger completion immediately.
   (company-idle-delay 0.1)
   ;; Number the candidates (use M-1, M-2 etc to select completions).
   (company-show-numbers t))

(use-package yaml-mode
  :mode(
       ("\\.yaml\\'" . yaml-mode)
       ("\\.yml\\'" . yaml-mode)))

(use-package python-mode
   :mode "\\.py\\'"
   :hook (python-mode . lsp-deferred)
   :config
   (setq python-indent-level 4))

   (use-package pyvenv
     :config
     (pyvenv-mode 1))

   (if (string= (system-name) clinton/personal-mac-name)
     (;(pyvenv-activate "/opt/homebrew/bin/python3")
      setq python-shell-interpreter "/opt/homebrew/bin/python3")
     (if (string= (system-name) clinton/work-mac-name)
       (pyvenv-activate "~/.pyenv/versions/3.7.3/envs/via-algo-shift-optimizer-3.7.3")))

;;   (setenv "PYTHONPATH" "/via/via-algo-supply-planning-utils")
   (require 'flycheck)
   (add-hook 'python-mode-hook
    (lambda ()
       (setq flycheck-python-pylint-executable "/usr/local/bin/pylint")
       (setq flycheck-pylintrc (substitute-in-file-name "~/.pylintrc"))))
   (setq lsp-diagnostic-package :none)
   (flycheck-add-next-checker 'python-pylint 'python-flake8)
   (setq compilation-scroll-output t)
   (use-package helm-make)
 ;(use-package company-lsp :commands company-lsp :ensure t)

 (use-package dockerfile-mode)

(setq multi-term-program "/bin/zsh")

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

               (concat acc
(format "- [[file:%s][%s]]\n"
          (file-relative-name (car it) "/Users/clinton/roam")
          (title-capitalization (replace-regexp-in-string "-" " " (replace-regexp-in-string "[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]-" "" (replace-regexp-in-string "-org" "" (org-roam--title-to-slug (file-relative-name (car it) "/Users/clinton/roam")))))))                           
                     )
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
(defun org-roam-to-hugo-md ()
  (interactive)
  ;; Make sure the author is set
  (setq user-full-name "Clinton Boys")

  (let ((files (mapcan
                (lambda (x) x)
                (org-roam-db-query
                [:select :distinct [files:file]
                 :from files
                 :left :outer :join tags :on (= files:file tags:file)
                 :where (like tags:tags '%public%)]))))
    (mapc
     (lambda (f)
       ;; Use temporary buffer to prevent a buffer being opened for
       ;; each note file.
       (with-temp-buffer
         (message "Working on: %s" f)
         (insert-file-contents f)

         (goto-char (point-min))
         ;; Add in hugo tags for export. This lets you write the
         ;; notes without littering HUGO_* tags everywhere
         ;; HACK:
         ;; org-export-output-file-name doesn't play nicely with
         ;; temp buffers since it attempts to get the file name from
         ;; the buffer. Instead we explicitely add the name of the
         ;; exported .md file otherwise you would get prompted for
         ;; the output file name on every note.
         (insert
          (format "#+HUGO_BASE_DIR: %s\n#+HUGO_SECTION: ./\n#+HUGO_SLUG: %s\n#+EXPORT_FILE_NAME: %s\n"
                  org-roam-publish-path
                  (file-path-to-slug (file-relative-name f "/Users/clinton/roam"))
                  (file-path-to-md-file-name f)))

         ;; If this is a placeholder note (no content in the
         ;; body) then add default text. This makes it look ok when
         ;; showing note previews in the index and avoids a headline
         ;; followed by a headline in the note detail page.
         (if (eq (clinton/org-roam--extract-note-body f) nil)
             (progn
               (goto-char (point-max))
               (insert "\n/This note does not have a description yet./\n")))

         ;; Add in backlinks because
         ;; org-export-before-processing-hook won't be useful the
         ;; way we are using a temp buffer
         (let ((links (clinton/org-roam--backlinks-list f)))
           (unless (string= links "")
             (goto-char (point-max))
             (insert (concat "\n* Links to this note\n") links)))

         (org-hugo-export-to-md)))
     files)))

(use-package yasnippet
  :ensure t)
