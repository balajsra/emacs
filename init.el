(setq inhibit-startup-message t) ; Disable startup message

(scroll-bar-mode -1)  ; Diable visible scrollbar
(tool-bar-mode -1)    ; Disable the toolbar
(tooltip-mode -1)     ; Disable tooltips
(set-fringe-mode 10)  ; Give some breathing room

(menu-bar-mode -1)    ; Disable the menu bar

(setq visible-bell t) ; Set up the visible bell

;; Font setup
(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 120)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)

(setq package-archives
    '(
        ("melpa" . "https://melpa.org/packages/")
	    ; ("melpa-stable" . "https://stable.melpa.org/packages/")
	    ("org" . "https://orgmode.org/elpa/")
	    ("elpa" . "https://elpa.gnu.org/packages/")
    )
)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

; (use-package command-log-mode)

; Ivy Autocompletion
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
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
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1)
  )

;; Doom Modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 15)
  )

;; Doom Themes
(use-package doom-themes
  :init (load-theme 'doom-dracula t)
)

;; NOTE: The first time you load your configuration on a new machine, you'll
;; need to run the following command interactively so that mode line icons
;; display correctly:
;;
;; M-X all-the-icons-install-fonts
(use-package all-the-icons)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(forge evil-magit magit counsel-projectile projectile hydra evil-collection evil general helpful counsel ivy-rich which-key rainbow-delimiters doom-modeline use-package ivy command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
(add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Which Key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1)
)

;; Ivy Rich
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; Counsel
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

;; Helpful
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; General (for easy keybindings)
(use-package general
  :config
  (general-create-definer sb/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (sb/leader-keys
   "t"  '(:ignore t :which-key "toggles")
   "tc" '(comment-line :which-key "toggle comment")
   "s"  '(:ignore s :which-key "settings")
   "st" '(counsel-load-theme :which-key "choose theme")
   "ss" '(hydra-text-scale/body :which-key "scale text")
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

  ;; Use visual line motoins even outside of visual-line-mode buffers
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

;; Hydra
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t)
)

;; Projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects/Personal" "~/Projects/UMICH"))
  )
  (when (file-directory-p "~/.config")
    (setq projectile-project-search-path '("~/.config"))
  )
  (setq projectile-switch-project-action #'projectile-dired)
)

(use-package counsel-projectile
  :config (counsel-projectile-mode)
)

;; Magit
(use-package magit
  ;; :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
)

;; NOTE: evil-magit was removed from MELPA and now a part of evil-collection
;; (use-package evil-magit
;;   :after magit
;; )

;; OPTIONAL: This will add GitHub integration to magit, but requires some token setup
;; (use-package forge)
