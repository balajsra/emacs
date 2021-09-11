;; Font Sizes
(defvar sb/default-font-size 110)
(defvar sb/default-variable-font-size 110)
(defvar sb/modeline-height 10)

;; Font Names
(defvar sb/source-code-font "FiraCode Nerd Font")
(defvar sb/document-font "Cantarell")

;; Initialize package sources
(require 'package)

(setq package-archives '(
                         ("melpa" . "https://melpa.org/packages/")
                         ("gnu-elpa"  . "https://elpa.gnu.org/packages/")
                         ("nongnu-elpa"  . "https://elpa.nongnu.org/nongnu/")
                        )
)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents)
)

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package)
)

(require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :init
  (auto-package-update-maybe)
)

(setq inhibit-startup-message t) ; Disable startup message

(scroll-bar-mode -1)  ; Disable visible scrollbar
(tool-bar-mode -1)    ; Disable the toolbar
(tooltip-mode -1)     ; Disable tooltips
(set-fringe-mode 10)  ; Give some breathing room

(menu-bar-mode -1)    ; Disable the menu bar

(setq visible-bell t) ; Set up the visible bell

;; Line Numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(
                org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                )
        )
  (add-hook mode (lambda () (display-line-numbers-mode 0)))
)

;; Set the default font
(set-face-attribute 'default nil :font sb/source-code-font :height sb/default-font-size)

;; Set the fixed pitch font
(set-face-attribute 'fixed-pitch nil :font sb/source-code-font :height sb/default-font-size)

;; Set the variable pitch font
(set-face-attribute 'variable-pitch nil :font sb/document-font :height sb/default-font-size :weight 'regular)

(use-package fira-code-mode
  :custom
  ;; List of ligatures to turn off
  (fira-code-mode-disabled-ligatures '(
                                       "[]" "#{" "#(" "#_" "#_(" "x"
                                       )
  ) 
  ;; Enables fira-code-mode automatically for programming major modes
  :hook prog-mode
)

(setq-default tab-width 4)

(defun reload-config ()
  (interactive)
  (load-file user-init-file)
  (revert-buffer t t)
)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; General (for easy keybindings)
(use-package general
  :config
  (general-create-definer sb/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"
  )

  (sb/leader-keys
    "o"  '(:ignore t :which-key "org-mode quick access")
    "oa" '(org-agenda :which-key "org agenda")
    "oc" '(org-capture :which-key "org capture")
    ;; "t"  '(:ignore t :which-key "toggles")
    ;; "tc" '(comment-line :which-key "toggle comment")
    "s"  '(:ignore s :which-key "settings")
    "sr" '(reload-config :which-key "reload config")
    "ss" '(hydra-text-scale/body :which-key "scale text")
    "st" '(counsel-load-theme :which-key "choose theme")
  )
)

(general-define-key
 "C-M-j" 'counsel-switch-buffer
)

;; Evil Keybindings
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
)

;; Easymotion
(use-package evil-easymotion
  :config
  (evilem-default-keybindings "SPC")
)

;; Dracula Doom Theme
(use-package doom-themes
  :init (load-theme 'doom-dracula t)
)

;; ;; Dracula Pro Theme
;; (add-to-list 'custom-theme-load-path "~/.config/emacs/themes")
;; (load-theme 'dracula-pro t)

;; Doom Modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height sb/modeline-height))
)

;; Emacs Dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  :init
  ;; Banner Title
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  ;; Banner Logo
  (setq dashboard-startup-banner 'logo)
  ;; Widgets
  (setq dashboard-items '((recents . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (registers . 5)))
  ;; Show Heading & File Icons
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  ;; Show info about packages loaded and init time
  (setq dashboard-set-init-info t)
  ;; Use with counsel-projectile
  (setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1)
)

;; Ivy Autocompletion
(use-package ivy
  :diminish
  :bind (
         ("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill)
        )
  :config
  (ivy-mode 1)
)

;; All the Icons Ivy Rich
(use-package all-the-icons-ivy-rich
  :ensure t
  :after counsel-projectile
  :init (all-the-icons-ivy-rich-mode 1)
)

;; Ivy Rich
(use-package ivy-rich
  :ensure t
  :init (ivy-rich-mode 1)
)

;; Counsel
(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1)
)

;; Helpful
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key)
)

;; Hydra
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t)
)

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :hook (text-mode . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'fill)
  (setq highlight-indent-guides-responsive 'stack)
  (setq highlight-indent-guides-delay 0)
)

(defun sb/org-font-setup ()
  ;; ;; Replace list hyphen with dot
  ;; (font-lock-add-keywords 'org-mode
  ;;                         '(("^ *\\([-]\\) "
  ;;                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face))
  )

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
)

(defun sb/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)
  (setq org-edit-src-content-indentation 0)
)

(use-package org
  :hook (org-mode . sb/org-mode-setup)
  :config
  ;; Change elipsis to down arrow
  (setq org-ellipsis " ▾")

  ;; Org Agenda Settings
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  ;; Org Agenda Files
  (setq org-agenda-files
        '(
          "~/OrgFiles/Tasks.org"
          ;; "~/OrgFiles/Habits.org"
          ;; "~/OrgFiles/Birthdays.org"
         )
  )

  ;; Org Habit Tracker
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  ;; Org TODO Keywords
  (setq org-todo-keywords
        '(
          (sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")
         )
  )

  ;; ;; Move tasks between documents
  ;; (setq org-refile-targets
  ;;   '(("Archive.org" :maxlevel . 1)
  ;;     ("Tasks.org" :maxlevel . 1)))

  ;; ;; Save Org buffers after refiling!
  ;; (advice-add 'org-refile :after 'org-save-all-org-buffers)

  ;; ;; Org Tags
  ;; (setq org-tag-alist
  ;;   '((:startgroup)
  ;;      ; Put mutually exclusive tags here
  ;;      (:endgroup)
  ;;      ("@errand" . ?E)
  ;;      ("@home" . ?H)
  ;;      ("@work" . ?W)
  ;;      ("agenda" . ?a)
  ;;      ("planning" . ?p)
  ;;      ("publish" . ?P)
  ;;      ("batch" . ?b)
  ;;      ("note" . ?n)
  ;;      ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ;; ("W" "Work Tasks" tags-todo "+work-email")

    ;; ;; Low-effort next actions
    ;; ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
    ;;  ((org-agenda-overriding-header "Low Effort Tasks")
    ;;   (org-agenda-max-todos 20)
    ;;   (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files))))))
  )

  ;; Org Capture Templates
  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/OrgFiles/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/OrgFiles/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/OrgFiles/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ;; ("w" "Workflows")
      ;; ("we" "Checking Email" entry (file+olp+datetree "~/OrgFiles/Journal.org")
      ;;      "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ;; ("m" "Metrics Capture")
      ;; ("mw" "Weight" table-line (file+headline "~/OrgFiles/Metrics.org" "Weight")
      ;;  "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)
     )
  )

  ;; ;; Keybinding for going directly to journal entry
  ;; (define-key global-map (kbd "C-c j")
  ;;   (lambda () (interactive) (org-capture nil "jj")))

  (sb/org-font-setup)
)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
  :init
  (setq inhibit-compacting-font-caches t)
)

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (makefile . t)
    (python . t))
)

(push '("conf-unix" . conf-unix) org-src-lang-modes)

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("hs" . "src haskell"))
(add-to-list 'org-structure-template-alist '("cf" . "src config"))

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines)
)

(use-package fish-mode)

(use-package gitattributes-mode)

(use-package gitconfig-mode)

(use-package gitignore-mode)

(use-package haskell-mode)

(use-package vimrc-mode)

(use-package cc-mode)

(defun sb/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode)
)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . sb/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l") ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t)
)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom)
)

(use-package lsp-treemacs
  :after lsp
)

(use-package lsp-ivy)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
)

(use-package company-box
  :hook (company-mode . company-box-mode)
)

;; Projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-project-search-path '("~/Projects/" "~/.config/" "~/.xmonad/"))
  (setq projectile-switch-project-action #'projectile-dired)
)

(use-package counsel-projectile
  :config (counsel-projectile-mode)
)

;; Magit
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
)

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
)
