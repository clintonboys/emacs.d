(setq custom-file (concat user-emacs-directory "/custom.el"))

(defvar clinton/personal-mac-name "clinton.local")
(defvar clinton/work-mac-name "Clinton-Boys-MacBook-Pro.local")

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

(defvar clinton/default-font-size 140)
(defvar clinton/default-variable-font-size 160)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)          ; Disable visible scrollbar
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
    "d" 'deft
    "w" 'count-words)

  (clinton/leader-l
    "d" 'xref-find-definitions
    "r" 'xref-find-references
    "t" '(counsel-load-theme :which-key "choose theme"))

(use-package doom-themes
  :init (load-theme 'doom-one t))

(use-package all-the-icons)

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

(add-hook 'after-init-hook 'global-company-mode)

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

  (setq org-agenda-files
        '("~/Dropbox/org/inbox.org"
          "~/Dropbox/org/technical.org"
          "~/Dropbox/org/creative.org"
          "~/Dropbox/org/personal.org"
          "~/Dropbox/org/projects.org"
          "~/Dropbox/org/lists.org"
          "~/Dropbox/org/gmail_cal.org"
          "~/Dropbox/org/icloud_cal.org"))

 (require 'org-habit)
 (add-to-list 'org-modules 'org-habit)
 (setq org-habit-graph-column 60)

 (setq org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
     (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

 (setq org-refile-targets
       '((org-agenda-files :maxlevel . 3)))
 (setq org-refile-use-outline-path 'file)
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

 (setq org-agenda-custom-commands
     '(("a" "Agenda"
    ((agenda ""
             ((org-agenda-span
               (quote day))
              (org-deadline-warning-days 14)))
     (todo "TODO"
           ((org-agenda-overriding-header "To Refile")
            (org-agenda-files
             (quote
              ("/Users/clinton/Dropbox/org/inbox.org")))))
     (todo "NEXT"
           ((org-agenda-overriding-header "Projects")
            (org-agenda-files
             (quote
              ("/Users/clinton/Dropbox/org/projects.org"))))))))
     )

(use-package org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  ("C-c t" . journal-file-today)
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

(if (string= (system-name) clinton/personal-mac-name)
(use-package org-roam
        :hook 
        (after-init . org-roam-mode)
        :custom
        (org-roam-directory "~/roam")
        :bind (:map org-roam-mode-map
                (("C-c n l" . org-roam)
                 ("C-c n f" . org-roam-find-file)
                 ("C-c n b" . org-roam-switch-to-buffer)
                 ("C-c n g" . org-roam-show-graph))
                :map org-mode-map
                (("C-c i" . org-roam-insert))))

(setq org-roam-db-location "/Users/clinton/org-roam.db"))

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
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . clinton/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show))

(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))


(use-package company
  :after lsp-mode
  :hook
  (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package python-mode
:mode "\\.py\\'"
:hook (python-mode . lsp-deferred)
:config
(setq python-indent-level 4))

(use-package pyvenv
  :config
  (pyvenv-mode 1))

(if (string= (system-name) clinton/personal-mac-name)
  (pyvenv-activate "~/.pyenv/versions/emacs-3.7")
  (if (string= (system-name) clinton/work-mac-name)
    (pyvenv-activate "~/.pyenv/versions/3.7.3/envs/via-algo-shift-optimizer-3.7.3")))
