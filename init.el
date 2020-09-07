;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Install a theme package. 
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))
(require 'org)
(add-to-list 'org-modules 'org-habit t)

;; Helm
(use-package helm
  :ensure t
  :init
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-candidate-number-list 50))

;; elpy
(use-package elpy
  :ensure t
  :init
  (elpy-enable))

;; which-key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))

;; Custom keybinding
(use-package general
  :ensure t
  :config (general-define-key
  :states '(normal visual insert emacs)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  "SPC" '(helm-M-x :which-key "M-x")
  ))

;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(epa-file-select-keys (quote silent))
 '(org-agenda-custom-commands
   (quote
    (("l" "Lists"
      ((tags-todo "books" nil)
       (tags "tv" nil)
       (tags "films" nil))
      nil nil)
     ("t" "All TODOs"
      ((tags-todo "personal"
		  ((org-agenda-category-filter-preset
		    (quote
		     ("TODO" "NEXT")))))
       (tags-todo "technical" nil)
       (tags-todo "creative" nil))
      nil)
     ("a" "Agenda"
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
		("/Users/clinton/Dropbox/org/projects.org"))))))))))
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/inbox.org" "~/Dropbox/org/technical.org" "~/Dropbox/org/creative.org" "~/Dropbox/org/personal.org" "~/Dropbox/org/projects.org" "~/Dropbox/org/lists.org" "~/Dropbox/org/gmail_cal.org" "~/Dropbox/org/icloud_cal.org"
     )))
 '(org-crypt-key "clintonboys@icloud.com")
 '(org-journal-date-format "%Y-%m-%d")
 '(org-journal-date-prefix "#+title: ")
 '(org-journal-dir
   "/Users/clinton/Library/Mobile Documents/iCloud~is~workflow~my~workflows/Documents/org-roam/")
 '(org-journal-file-format "%Y-%m-%d.org")
 '(org-refile-targets
   (quote
    ((org-agenda-files :tag . "~/org/personal.org")
     (org-agenda-files :tag . "~/org/creative.org")
     (org-agenda-files :tag . "~/org/technical.org"))))
 '(org-roam-directory
   "/Users/clinton/Library/Mobile Documents/iCloud~is~workflow~my~workflows/Documents/org-roam/")
 '(package-selected-packages
   (quote
    (smart-mode-line-atom-one-dark-theme olivetti ace-window org-roam-bibtex lsp-python-ms lsp-ui company-lsp lsp-mode ivy-posframe magit doom-modeline buffer-move smart-mode-line dired-subtree elpy auto-complete exec-path-from-shell markdown-mode general which-key helm use-package doom-themes))))
; projectile

(setq org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#282c34" :foreground "#bbc2cf" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "IBM Plex Mono"))))
 '(org-level-1 ((t (:foreground "#BF9D7A"))))
 '(org-level-2 ((t (:foreground "#E4E9CD"))))
 '(org-level-3 ((t (:foreground "#EBF2EA"))))
 '(org-level-4 ((t (:foreground "#0ABDA0"))))
 '(org-level-5 ((t (:foreground "#80ADD7")))))
(setq-default line-spacing 0.45)

(global-display-line-numbers-mode)

(custom-theme-set-faces 'user
                        `(org-level-1 ((t (:foreground "#BF9D7A"))))) ;;D8A47F
(custom-theme-set-faces 'user
                        `(org-level-2 ((t (:foreground "#E4E9CD"))))) ;;EF8354
(custom-theme-set-faces 'user
                        `(org-level-3 ((t (:foreground "#EBF2EA"))))) ;;EE4B6A
(custom-theme-set-faces 'user
                        `(org-level-4 ((t (:foreground "#0ABDA0"))))) ;;DF3B57
(custom-theme-set-faces 'user
                        `(org-level-5 ((t (:foreground "#80ADD7"))))) ;;0F7173


(global-display-line-numbers-mode)
(setq display-line-numbers-width 4)  ;; workaround for annoying issue of shifting line number wid
(add-hook 'org-agenda-mode-hook (lambda () (display-line-numbers-mode -1)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(global-visual-line-mode t)

(setq org-default-notes-file "/Users/clinton/Dropbox/org/inbox.org")

(setq org-refile-targets '(("/Users/clinton/Dropbox/org/main.org" :maxlevel . 5)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (ditaa . t)
   (dot . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (haskell . nil)
   (latex . t)
   (ledger . t)         ;this is the important one for this tutorial
   (ocaml . nil)
   (octave . t)
   (python . t)
   (ruby . t)
   (screen . nil)
   (shell . t)
   (sql . nil)
   (sqlite . t)))
(require 'rust-mode)

(setq org-babel-python-command "/Users/clinton/.pyenv/versions/emacs-3.7/bin/python3")

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


(global-auto-complete-mode t)

(require 'auto-complete)
(global-auto-complete-mode t)

(setq org-default-notes-file "/Users/clinton/Dropbox/org/inbox.org")

(setq org-agenda-start-on-weekday 0)

(setq python-shell-interpreter "/Users/clinton/.pyenv/versions/emacs-3.7/bin/python3")

;(pyenv-mode)

(global-set-key (kbd "C-c C-s") 'org-schedule)

(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))

(defun mhj/dwim-toggle-or-open ()
  "Toggle subtree or open the file."
  (interactive)
  (if (file-directory-p (dired-get-file-for-visit))
      (progn
    (dired-subtree-toggle)
    (revert-buffer))
    (dired-find-file)))

(defun mhj/mouse-dwim-to-toggle-or-open (event)
  "Toggle subtree or the open file on mouse-click in dired."
  (interactive "e")
  (let* ((window (posn-window (event-end event)))
     (buffer (window-buffer window))
     (pos (posn-point (event-end event))))
    (progn
      (with-current-buffer buffer
    (goto-char pos)
    (mhj/dwim-toggle-or-open)))))

(use-package dired-subtree
  :demand
  :bind
  (:map dired-mode-map
    ("<enter>" . mhj/dwim-toggle-or-open)
    ("<return>" . mhj/dwim-toggle-or-open)
    ("<tab>" . mhj/dwim-toggle-or-open)
    ("<down-mouse-1>" . mhj/mouse-dwim-to-toggle-or-open))
  :config
  (progn
    ;; Function to customize the line prefixes (I simply indent the lines a bit)
    (setq dired-subtree-line-prefix (lambda (depth) (make-string (* 2 depth) ?\s)))
    (setq dired-subtree-use-backgrounds nil)))

(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode))

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

(global-set-key (kbd "C-c w") 'count-words)
(global-set-key (kbd "C-c i") 'org-roam-insert)
(global-set-key (kbd "C-c s") 'org-schedule)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(add-hook 'auto-save-hook 'org-save-all-org-buffers)

 (defface egoge-display-time
   '((((type x w32 mac))
      ;; #060525 is the background colour of my default face.
      (:foreground "#060525" :inherit bold))
     (((type tty))
      (:foreground "blue")))
   "Face used to display the time in the mode line.")

(use-package epa-file
    :ensure nil ;; included with Emacs
    :config
    (setq epa-file-encrypt-to '("clintonboys@icloud.com"))
;    (epa-file-enable)
    :custom
    (epa-file-select-keys 'silent))

(use-package org-crypt
  :ensure nil  ;; included with org-mode
  :after org
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  :custom
  (org-crypt-key "clintonboys@icloud.com"))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

(server-start)
(add-to-list 'load-path "~/path/to/org/protocol/")
(require 'org-protocol)

 (setq-default left-margin-width 300 right-margin-width 300) ; Define new widths.
 (set-window-buffer nil (current-buffer)) ; Use them now.

;(require 'epa-file)
;(epa-file-enable)
(require 'org-roam)

(use-package org-roam
        :hook 
        (after-init . org-roam-mode)
        ;; :straight (:host github :repo "jethrokuan/org-roam")
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


(require 'deft)
(global-set-key "\C-cd" 'deft)
(add-hook 'deft-mode-hook (lambda () (visual-line-mode 0)))
(setq-default truncate-lines t)
(setq deft-directory "/Users/clinton/Library/Mobile Documents/iCloud~is~workflow~my~workflows/Documents/org-roam/")

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

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package pyvenv
  :ensure t
  :init
  (setenv "WORKON_HOME" "~/.pyenv/versions"))

(pyvenv-activate "~/.pyenv/versions/emacs-3.7")

(use-package lsp-mode
  :config
  (require 'lsp-clients)
  (add-hook 'python-mode-hook 'lsp))
(use-package company-lsp)
(use-package lsp-ui)

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))  ; or lsp-deferred
(require 'olivetti)

(require 'hydra)

(defhydra hydra-transpose ()
  "transposing hydra"
  ("l" transpose-lines "lines")
  ("w" transpose-words "words")
  ("s" transpose-sexps "sexps")
  ("p" transpose-paragraphs "paragraphs")
  ("c" transpose-chars "characters")
  ("w" transpose-frame "windows"))
(global-set-key (kbd "C-t") 'hydra-transpose/body)

(defhydra hydra-roam ()
  "org-roam"
  ("b" org-roam-switch-to-buffer "switch to buffer")
  ("f" org-roam-find-file "find file")
  ("l" org-roam "backlinks buffer"))
(global-set-key (kbd "C-c r") 'hydra-roam/body)

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("python"))))
;;; Unbind 'C-x f'
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

(global-unset-key "\C-xf")

(global-set-key (kbd "C-x o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(global-set-key (kbd "C-c q") 'org-toggle-inline-images)
(global-set-key (kbd "C-c u") 'run-python)
(global-set-key (kbd "C-c l") 'load-file)
(global-set-key (kbd "\C-c g") 'ivy-goto-line)
(global-set-key (kbd "\C-x g") 'magit)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;(require 'notmuch)

(define-key global-map [remap list-buffers] 'buffer-menu-other-window)

(use-package smart-mode-line-atom-one-dark-theme
   :ensure t
   :config
    (sml/setup)
)

  (setq calendar-and-diary-frame-parameters
        '((name . "Calendar") (title . "Calendar")
          (height . 20) (width . 78)
          (minibuffer . t)))
