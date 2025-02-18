(add-to-list 'load-path "~/.emacs.d/scripts")
(require 'elpaca-setup)  ;; The Elpaca Package Manager
(require 'app-launchers) ;; Use emacs as a run launcher like dmenu (experimental)
(require 'buffer-move)   ;; Buffer-move for better window management
(require 'eshell-prompt) ;; A fancy prompt for eshell

(setq mzn/is-windows (eq system-type 'windows-nt))

(defvar mzn/unix-font-size 200)
(defvar mzn/windows-font-size 120) ;; Ensure this variable is defined
(defvar default-font-size (if mzn/is-windows
                              mzn/windows-font-size
                            mzn/unix-font-size))

(defvar mzn/unix-dotfiles-path "~/Documents/dotfiles")
(defvar mzn/unix-notes-path "~/Documents/notes")
(defvar mzn/unix-agenda-path "~/Documents/workflows")
(defvar mzn/unix-code-path "~/Documents/code")

;; Define path and font size in Windows
(defvar mzn/windows-dotfiles-path nil)
(defvar mzn/windows-notes-path nil)
(defvar mzn/windows-agenda-path nil)
(defvar mzn/windows-code-path nil)
(defvar mzn/windows-font-size 120) ;; Ensure this variable is defined

;; Get environment variables and assign values
(let ((my-custom-path (getenv "MZN_WORK")))
  (when my-custom-path
    (setq mzn/windows-dotfiles-path (concat (file-name-as-directory my-custom-path) "dotfiles"))
    (setq mzn/windows-notes-path (concat (file-name-as-directory my-custom-path) "notes"))
    (setq mzn/windows-agenda-path (concat (file-name-as-directory my-custom-path) "workflows"))
    (setq mzn/windows-code-path (concat (file-name-as-directory my-custom-path) "code"))))

(defvar default-variable-font-size (if mzn/is-windows
                                       mzn/windows-font-size
                                     mzn/unix-font-size))

;; Set paths based on the operating system type
(defvar mzn/dotfiles-path (if mzn/is-windows
                              mzn/windows-dotfiles-path
                            mzn/unix-dotfiles-path))

(defvar mzn/notes-path (if mzn/is-windows
                           mzn/windows-notes-path
                         mzn/unix-notes-path))

(defvar mzn/agenda-path (if mzn/is-windows
                            mzn/windows-agenda-path
                          mzn/unix-agenda-path))

(defvar mzn/code-path (if mzn/is-windows
                          mzn/windows-code-path
                        mzn/unix-code-path))

(use-package transient)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Expands to: (elpaca evil (use-package evil :demand t))
(use-package evil
    :init      ;; tweak evil's configuration before loading it
    (setq evil-want-integration t  ;; This is optional since it's already set to t by default.
          evil-want-keybinding nil
          evil-vsplit-window-right t
          evil-split-window-below t
          evil-undo-system 'undo-redo)  ;; Adds vim-like C-r redo functionality
    (evil-mode))

(use-package evil-collection
  :after evil
  :config
  ;; Do not uncomment this unless you want to specify each and every mode
  ;; that evil-collection should works with.  The following line is here 
  ;; for documentation purposes in case you need it.  
  ;; (setq evil-collection-mode-list '(calendar dashboard dired ediff info magit ibuffer))
  (add-to-list 'evil-collection-mode-list 'help) ;; evilify help mode
  (evil-collection-init))

;; Using RETURN to follow links in Org/Evil 
;; Unmap keys in 'evil-maps if not done, (setq org-return-follows-link t) will not work
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))
;; Setting RETURN key in org-mode to follow links
  (setq org-return-follows-link  t)

(use-package evil-escape
  :ensure t
  :config
  (setq-default evil-escape-key-sequence "jk") ;; Push "jk" back normal from insert 
  (setq evil-escape-delay 0.5) ;; delay time is 0.5s
  (evil-escape-mode 1))

(require 'general-keybindings)

(use-package git-timemachine
  :after git-timemachine
  :hook (evil-normalize-keymaps . git-timemachine-hook)
  :config
    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-j") 'git-timemachine-show-previous-revision)
    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-k") 'git-timemachine-show-next-revision)
)

(use-package magit)

(use-package all-the-icons
  :ensure t
  :config
  (set-fontset-font t 'symbol "all-the-icons" nil 'prepend))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(set-fontset-font t 'symbol "all-the-icons" nil 'prepend)

(use-package dashboard
  :ensure t 
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-buffer-name "*mzn/emacs*")
  (setq dashboard-footer-messages '("Dashboard is pretty cool!"))
  (setq dashboard-banner-logo-title "EMACS - Enjoy Programming & Writing!")
  (setq dashboard-startup-banner "~/.emacs.d/images/logo.png")  ;; use custom image as banner
  (setq dashboard-set-navigator t)
  (setq dashboard-items '((recents . 5)
                          (projects . 5)
                          (agenda . 5 )
                          (bookmarks . 3)))
(setq dashboard-heading-shorcut-format " [%s]")
(setq dashboard-item-shortcuts '((recents   . "r")
                                 (projects  . "p")
                                 (agenda    . "a")
                                 (bookmarks . "m")))
 (setq dashboard-icon-type 'all-the-icons)  ; use `all-the-icons' package
 (setq dashboard-projects-backend 'projectile)
  :custom 
  (dashboard-modify-heading-icons '((recents . "file-text")
				      (bookmarks . "book")))
  :config
  (setq dashboard-page-separator "\n\f\n")
  (setq dashboard-center-content t ;; set to 't' for centered content
        dashboard-vertically-center-content t)
  (dashboard-setup-startup-hook))

(use-package diminish)

(use-package dired-open
  :config
  (setq dired-open-extensions '(("gif" . "sxiv")
                                ("jpg" . "sxiv")
                                ("png" . "sxiv")
                                ("mkv" . "mpv")
                                ("mp4" . "mpv"))))

(use-package peep-dired
  :after dired
  :hook (evil-normalize-keymaps . peep-dired-hook)
  :config
    (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
    (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
    (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
    (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file)
)

(use-package drag-stuff
  :init
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

(defun dt-ediff-hook ()
  (ediff-setup-keymap)
  (define-key ediff-mode-map "j" 'ediff-next-difference)
  (define-key ediff-mode-map "k" 'ediff-previous-difference))

(add-hook 'ediff-mode-hook 'dt-ediff-hook)

(set-face-attribute 'default nil
                    :family "Fira Code"
                    :height default-font-size
                    :weight 'regular)
(set-face-attribute 'variable-pitch nil
                    :family "Cantarell"
                    :height default-font-size
                    :weight 'regular)
(set-face-attribute 'fixed-pitch nil
                    :family "Fira Code"
                    :height default-font-size
                    :weight 'light)
;; for code comment
(set-face-attribute 'font-lock-comment-face nil
                    :family "Cantarell"
                    :weight 'light
                    :slant 'normal)
;; for keyword
(set-face-attribute 'font-lock-keyword-face nil
                    :family "Cantarell"
                    :weight 'bold)
(set-face-attribute 'widget-button nil
                    :family "Fira Code"
                    :weight 'normal)
(set-face-attribute 'font-lock-doc-face nil
                    :family "Cantarell"
                    :weight 'light
                    :slant 'normal)
(set-face-attribute 'org-block nil
                    :family "Fira Code"
                    :weight 'normal
                    :slant 'normal)

;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.
;;(add-to-list 'default-frame-alist '(font . "Fira Code"))

;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 35      ;; sets modeline height
        doom-modeline-bar-width 5    ;; sets right bar width
        doom-modeline-persp-name t   ;; adds perspective name to modeline
        doom-modeline-persp-icon t)) ;; adds folder icon next to persp name

(use-package doom-themes
  :init (load-theme 'doom-dracula t))

;;(add-to-list 'default-frame-alist '(alpha-background . 80)) ; For all new frames henceforth

(use-package hl-todo
  :hook ((org-mode . hl-todo-mode)
         (prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

(use-package counsel
  :after ivy
  :diminish
  :config 
    (counsel-mode)
    (setq ivy-initial-inputs-alist nil)) ;; removes starting ^ regex in M-x

(use-package ivy
  :bind
  (("C-s" . swiper)
  ("C-c C-r" . ivy-resume)
  ("C-x B" . ivy-switch-buffer-other-window)
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
  :diminish
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode))

(use-package ivy-posframe
  :after ivy
  :config
  (setq ivy-posframe-display-functions-alist
	'((swiper          . ivy-posframe-display-at-frame-center)
	  (complete-symbol . ivy-posframe-display-at-point)
	  (t               . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-parameters
	'((left-fringe . 10)
	  (right-fringe . 10)))
  (ivy-posframe-mode 1))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :after ivy
  :ensure t
  :init (ivy-rich-mode 1)) ;; this gets us descriptions in M-x.

(setq org-agenda-files (list (concat mzn/agenda-path "/agenda.org")))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(eval-after-load 'org-indent '(diminish 'org-indent-mode))

(defun mzn/org-font-setup ()
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.05)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  ;;(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(with-eval-after-load 'org
  (mzn/org-font-setup))

(dolist (character '(?\x25C9 ?\x25CB ?\x2738 ?\x273F))
  (set-fontset-font nil character "Fira Code"))

(require 'org-tempo)

(defun org-mode-visual-fill ()
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . org-mode-visual-fill))

(setq org-src-preserve-indentation t)

(use-package toc-org
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))

(use-package projectile
  :config
  (projectile-mode 1))

(delete-selection-mode 1)    ;; You can select text and delete it by typing.
(electric-indent-mode -1)    ;; Turn off the weird indenting that Emacs does by default.
(electric-pair-mode 1)       ;; Turns on automatic parens pairing
;; The following prevents <> from auto-pairing when electric-pair-mode is on.
;; Otherwise, org-tempo is broken when you try to <s TAB...
(add-hook 'org-mode-hook (lambda ()
           (setq-local electric-pair-inhibit-predicate
                   `(lambda (c)
                  (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
(global-auto-revert-mode t)  ;; Automatically show changes if the file has changed
(global-display-line-numbers-mode 1) ;; Display line numbers
(global-visual-line-mode t)  ;; Enable truncated lines
(menu-bar-mode -1)           ;; Disable the menu bar 
(scroll-bar-mode -1)         ;; Disable the scroll bar
(tool-bar-mode -1)           ;; Disable the tool bar
(setq org-edit-src-content-indentation 0) ;; Set src block automatic indent to 0 instead of 2.
(setq use-file-dialog nil)   ;; No file dialog
(setq use-dialog-box nil)    ;; No dialog box
(setq pop-up-windows nil)    ;; No popup windows
;; Maximized the emacs windows when it start
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Set frame transparency
(defvar mzn/frame-transparency '(97 . 97))
(set-frame-parameter (selected-frame) 'alpha mzn/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,mzn/frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package tldr)

(use-package which-key
  :init
    (which-key-mode 1)
  :diminish
  :config
  (setq which-key-side-window-location 'bottom
	  which-key-sort-order #'which-key-key-order-alpha
	  which-key-allow-imprecise-window-fit nil
	  which-key-sort-uppercase-first nil
	  which-key-add-column-padding 1
	  which-key-max-display-columns nil
	  which-key-min-display-lines 6
	  which-key-side-window-slot -10
	  which-key-side-window-max-height 0.25
	  which-key-idle-delay 0.8
	  which-key-max-description-length 25
	  which-key-allow-imprecise-window-fit nil
	  which-key-separator " → " ))
