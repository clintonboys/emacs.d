(setq custom-file (concat user-emacs-directory "/custom.el"))

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
(defvar clinton/default-variable-font-size 140)

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
(set-face-attribute 'variable-pitch nil :font "Fira Code" :height clinton/default-variable-font-size :weight 'light)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-set-key (kbd "C-c w") 'count-words)
(global-set-key (kbd "C-x /") 'comment-region)
(global-set-key (kbd "C-c a") 'org-agenda)

;;;*** which-key

(use-package which-key
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; (use-package general
;;   :config
;;   (general-create-definer efs/leader-keys
;;     :keymaps '(normal insert visual emacs)
;;     :prefix "SPC"
;;     :global-prefix "C-SPC")

;;   (efs/leader-keys
;;     "t"  '(:ignore t :which-key "toggles")
;;     "tt" '(counsel-load-theme :which-key "choose theme")))

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

(custom-set-faces
 '(default ((t (:foreground "#bbc2cf"))))
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
              ("/Users/clinton/Dropbox/org/projects.org")))))))))

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

(if (not (string= (system-name) "Clinton-Boys-MacBook-Pro.local"))
    ; Do not load org-roam on my work machine
  (use-package org-roam
          :hook 
          (after-init . org-roam-mode)
          :custom
          (org-roam-directory "/Users/clinton/Library/Mobile Documents/iCloud~is~workflow~my~workflows/Documents/org-roam/")
          :bind (:map org-roam-mode-map
                  (("C-c n l" . org-roam)
                   ("C-c n f" . org-roam-find-file)
                   ("C-c n b" . org-roam-switch-to-buffer)
                   ("C-c n g" . org-roam-show-graph))
                  :map org-mode-map
                  (("C-c i" . org-roam-insert))))  

  (setq org-roam-db-location "/Users/clinton/org-roam.db")
  (add-hook 'after-init-hook 'ivy-mode)
  (setq org-roam-capture-templates
    '(("d" "default" plain (function org-roam-capture--get-point)
       "%?"
       :file-name "%<%Y%m%d%H%M%S>-${slug}"
       :head "#+TITLE: ${title}\n"
       :unnarrowed t)))

  (use-package 'deft)
  (global-set-key "\C-cd" 'deft)
  (add-hook 'deft-mode-hook (lambda () (visual-line-mode 0)))
  (setq-default truncate-lines t)
  (setq deft-directory "/Users/clinton/Library/Mobile Documents/iCloud~is~workflow~my~workflows/Documents/org-roam/")

  (org-reload)
)
