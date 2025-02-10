(add-to-list 'load-path "~/.emacs.d/scripts")

(require 'elpaca-setup)  ;; The Elpaca Package Manager
(require 'app-launchers) ;; Use emacs as a run launcher like dmenu (experimental)
(require 'buffer-move)   ;; Buffer-move for better window management
(require 'eshell-prompt) ;; A fancy prompt for eshell

;; Detect the OS type
(setq mzn/is-windows (eq system-type 'windows-nt))

;; Define path and font size in Unix
(defvar mzn/unix-dotfiles-path "~/Documents/dotfiles")
(defvar mzn/unix-notes-path "~/Documents/notes")
(defvar mzn/unix-agenda-path "~/Documents/workflows")
(defvar mzn/unix-code-path "~/Documents/code")
(defvar mzn/unix-font-size 200)

;; Define path and font size in Windows
(defvar mzn/windows-dotfiles-path nil)
(defvar mzn/windows-notes-path nil)
(defvar mzn/windows-agenda-path nil)
(defvar mzn/windows-code-path nil)
(defvar mzn/windows-font-size 140) ;; Ensure this variable is defined

;; Get environment variables and assign values
(let ((my-custom-path (getenv "MZN_WORK")))
  (when my-custom-path
    (setq mzn/windows-dotfiles-path (concat (file-name-as-directory my-custom-path) "dotfiles"))
    (setq mzn/windows-notes-path (concat (file-name-as-directory my-custom-path) "notes"))
    (setq mzn/windows-agenda-path (concat (file-name-as-directory my-custom-path) "workflows"))
    (setq mzn/windows-code-path (concat (file-name-as-directory my-custom-path) "code"))))

;; Set the default font size based on the operating system type
(defvar default-font-size (if mzn/is-windows
                              mzn/windows-font-size
                            mzn/unix-font-size))

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

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(setq backup-directory-alist '((".*" . "~/.local/share/Trash/files")))

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
  (setq dashboard-center-content t ;; set to 't' for centered content
        dashboard-vertically-center-content t)
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
  (dashboard-setup-startup-hook))

;; 设置 Cantarell 字体
(defun mzn/dashboard-font-setup ()
  (with-current-buffer "*mzn/emacs*"
    (setq buffer-face-mode-face '(:family "Fira Code Light" :height 100 :weight Light))
    (buffer-face-mode)))

(add-hook 'dashboard-mode-hook 'mzn/dashboard-font-setup)

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

(use-package elfeed
  :config
  (setq elfeed-search-feed-face ":foreground #ffffff :weight bold"
        elfeed-feeds (quote
                       (("https://www.reddit.com/r/linux.rss" reddit linux)
                        ("https://www.reddit.com/r/commandline.rss" reddit commandline)
                        ("https://www.reddit.com/r/distrotube.rss" reddit distrotube)
                        ("https://www.reddit.com/r/emacs.rss" reddit emacs)
                        ("https://www.gamingonlinux.com/article_rss.php" gaming linux)
                        ("https://hackaday.com/blog/feed/" hackaday linux)
                        ("https://opensource.com/feed" opensource linux)
                        ("https://linux.softpedia.com/backend.xml" softpedia linux)
                        ("https://itsfoss.com/feed/" itsfoss linux)
                        ("https://www.zdnet.com/topic/linux/rss.xml" zdnet linux)
                        ("https://www.phoronix.com/rss.php" phoronix linux)
                        ("http://feeds.feedburner.com/d0od" omgubuntu linux)
                        ("https://www.computerworld.com/index.rss" computerworld linux)
                        ("https://www.networkworld.com/category/linux/index.rss" networkworld linux)
                        ("https://www.techrepublic.com/rssfeeds/topic/open-source/" techrepublic linux)
                        ("https://betanews.com/feed" betanews linux)
                        ("http://lxer.com/module/newswire/headlines.rss" lxer linux)
                        ("https://distrowatch.com/news/dwd.xml" distrowatch linux)))))
 

(use-package elfeed-goodies
  :init
  (elfeed-goodies/setup)
  :config
  (setq elfeed-goodies/entry-pane-size 0.5))

(use-package ellama
  :init
  (setopt ellama-keymap-prefix "C-c e")  ;; keymap for all ellama functions
  (setopt ellama-language "English")     ;; language ellama should translate to
  (require 'llm-ollama)
  (setopt ellama-provider
	  (make-llm-ollama
	   ;; this model should be pulled to use it
	   ;; value should be the same as you print in terminal during pull
	   :chat-model "llama3.1"
	   :embedding-model "nomic-embed-text"
	   :default-chat-non-standard-params '(("num_ctx" . 8192))))
  ;; Predefined llm providers for interactive switching.
  (setopt ellama-providers
		    '(("zephyr" . (make-llm-ollama
				   :chat-model "zephyr"
				   :embedding-model "zephyr"))

		      ("llama3.1" . (make-llm-ollama
				   :chat-model "llama3.1"
				   :embedding-model "llama3.1"))
		      ("mixtral" . (make-llm-ollama
				    :chat-model "mixtral"
				    :embedding-model "mixtral"))))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  ;; Translation llm provider
  (setopt ellama-translation-provider (make-llm-ollama
				       :chat-model "mixtral"
				       :embedding-model "nomic-embed-text"))
  :config
  (setq ellama-sessions-directory "~/.emacs.d/ellama-sessions/"
        ellama-sessions-auto-save t))

(use-package eradio
  :init
  (setq eradio-player '("mpv" "--no-video" "--no-terminal"))
  :config
  (setq eradio-channels '(("Totally 80s FM" . "https://zeno.fm/radio/totally-80s-fm/")
                          ("Oldies Radio 50s-60s" . "https://zeno.fm/radio/oldies-radio-50s-60s/")
                          ("Oldies Radio 70s" . "https://zeno.fm/radio/oldies-radio-70s/")
                          ("Unlimited 80s" . "https://zeno.fm/radio/unlimited80s/")
                          ("80s Hits" . "https://zeno.fm/radio/80shits/")
                          ("90s Hits" . "https://zeno.fm/radio/90s_HITS/")
                          ("2000s Pop" . "https://zeno.fm/radio/2000s-pop/")
                          ("The 2000s" . "https://zeno.fm/radio/the-2000s/")
                          ("Hits 2010s" . "https://zeno.fm/radio/helia-hits-2010/")
                          ("Classical Radio" . "https://zeno.fm/radio/classical-radio/")
                          ("Classical Relaxation" . "https://zeno.fm/radio/radio-christmas-non-stop-classical/")
                          ("Classic Rock" . "https://zeno.fm/radio/classic-rockdnb2sav8qs8uv/")
                          ("Gangsta49" . "https://zeno.fm/radio/gangsta49/")
                          ("HipHop49" . "https://zeno.fm/radio/hiphop49/")
                          ("Madhouse Country Radio" . "https://zeno.fm/radio/madhouse-country-radio/")
                          ("PopMusic" . "https://zeno.fm/radio/popmusic74vyurvmug0uv/")
                          ("PopStars" . "https://zeno.fm/radio/popstars/")
                          ("RadioMetal" . "https://zeno.fm/radio/radio-metal/")
                          ("RocknRoll Radio" . "https://zeno.fm/radio/rocknroll-radio994c7517qs8uv/"))))

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
  (setq-default evil-escape-key-sequence "jk") ;; 按 "jk" 触发 escape
  (setq evil-escape-delay 0.5) ;; 设置延迟时间为 0.2 秒
  (evil-escape-mode 1))

(use-package flycheck
  :ensure t
  :defer t
  :diminish
  :init (global-flycheck-mode))

(set-face-attribute 'default nil
  :font "Fira Code Light"
  :height default-font-size
  :weight 'regular)
(set-face-attribute 'variable-pitch nil
  :font "Fira Code Light"
  :height default-font-size
  :weight 'regular)
(set-face-attribute 'fixed-pitch nil
  :font "Cantarell"
  :height default-font-size
  :weight 'regular)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.
;;(add-to-list 'default-frame-alist '(font . "JetBrains Mono-11"))

;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(use-package general
  :config
  (general-evil-setup)
  
  ;; set up 'SPC' as the global leader key
  (general-create-definer dt/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  (dt/leader-keys
    "SPC" '(counsel-M-x :wk "Counsel M-x")
    "." '(find-file :wk "Find file")
    "=" '(perspective-map :wk "Perspective") ;; Lists all the perspective keybindings
    "TAB TAB" '(comment-line :wk "Comment lines")
    "u" '(universal-argument :wk "Universal argument"))

   (dt/leader-keys
    "a" '(:ignore t :wk "A.I.")
    "a a" '(ellama-ask-about :wk "Ask ellama about region")
    "a e" '(:ignore t :wk "Ellama enhance")
    "a e g" '(ellama-improve-grammar :wk "Ellama enhance wording")
    "a e w" '(ellama-improve-wording :wk "Ellama enhance grammar")
    "a i" '(ellama-chat :wk "Ask ellama")
    "a p" '(ellama-provider-select :wk "Ellama provider select")
    "a s" '(ellama-summarize :wk "Ellama summarize region")
    "a t" '(ellama-translate :wk "Ellama translate region"))
   
  (dt/leader-keys
    "b" '(:ignore t :wk "Bookmarks/Buffers")
    "b b" '(ivy-switch-buffer :wk "Switch to buffer")
    "b c" '(clone-indirect-buffer :wk "Create indirect buffer copy in a split")
    "b C" '(clone-indirect-buffer-other-window :wk "Clone indirect buffer in new window")
    "b d" '(bookmark-delete :wk "Delete bookmark")
    "b i" '(ibuffer :wk "Ibuffer")
    "b k" '(kill-current-buffer :wk "Kill current buffer")
    "b K" '(kill-some-buffers :wk "Kill multiple buffers")
    "b l" '(list-bookmarks :wk "List bookmarks")
    "b m" '(bookmark-set :wk "Set bookmark")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer")
    "b R" '(rename-buffer :wk "Rename buffer")
    "b s" '(basic-save-buffer :wk "Save buffer")
    "b S" '(save-some-buffers :wk "Save multiple buffers")
    "b w" '(bookmark-save :wk "Save current bookmarks to bookmark file"))

   (dt/leader-keys
    "c" '(:ignore t :wk "Code")
    "c b" '(my-cmake-build :wk "Build Project")
    "c c" '(my-cmake-setup :wk "CMake Configure (For C/Cpp)")
    "c d" '(my-cmake-debug :wk "Debug")
    "c f" '(lsp-format-buffer :wk "Format code")
    "c r" '(my-cmake-run :wk "Run Executable")
    "c s" '(my-select-compiler :wk "Select Compiler"))
 
  (dt/leader-keys
    "d" '(:ignore t :wk "Dired")
    "d d" '(dired :wk "Open dired")
    "d f" '(wdired-finish-edit :wk "Writable dired finish edit")
    "d j" '(dired-jump :wk "Dired jump to current")
    "d n" '(neotree-dir :wk "Open directory in neotree")
    "d p" '(peep-dired :wk "Peep-dired")
    "d w" '(wdired-change-to-wdired-mode :wk "Writable dired"))

  (dt/leader-keys
    "e" '(:ignore t :wk "Ediff/Eshell/Eval/EWW")    
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e d" '(eval-defun :wk "Evaluate defun containing or after point")
    "e e" '(eval-expression :wk "Evaluate and elisp expression")
    "e f" '(ediff-files :wk "Run ediff on a pair of files")
    "e F" '(ediff-files3 :wk "Run ediff on three files")
    "e h" '(counsel-esh-history :which-key "Eshell history")
    "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
    "e n" '(eshell-new :wk "Create new eshell buffer")
    "e r" '(eval-region :wk "Evaluate elisp in region")
    "e R" '(eww-reload :which-key "Reload current page in EWW")
    "e s" '(eshell :which-key "Eshell")
    "e w" '(eww :which-key "EWW emacs web wowser"))

  (dt/leader-keys
    "f" '(:ignore t :wk "Files")    
    "f c" '((lambda () (interactive)
              (find-file "~/.emacs.d/config.org")) 
            :wk "Open emacs config.org")
    "f e" '((lambda () (interactive)
              (dired "~/.emacs.d")) 
            :wk "Open user-emacs-directory in dired")
    "f d" '(find-grep-dired :wk "Search for string in files in DIR")
    "f g" '(counsel-grep-or-swiper :wk "Search for string current file")
    "f i" '((lambda () (interactive)
              (find-file "~/.emacs.d/init.el")) 
            :wk "Open emacs init.el")
    "f j" '(counsel-file-jump :wk "Jump to a file below current directory")
    "f l" '(counsel-locate :wk "Locate a file")
    "f r" '(counsel-recentf :wk "Find recent files")
    "f u" '(sudo-edit-find-file :wk "Sudo find file")
    "f U" '(sudo-edit :wk "Sudo edit file"))

  (dt/leader-keys
    "g" '(:ignore t :wk "Git")    
    "g /" '(magit-displatch :wk "Magit dispatch")
    "g ." '(magit-file-displatch :wk "Magit file dispatch")
    "g b" '(magit-branch-checkout :wk "Switch branch")
    "g c" '(:ignore t :wk "Create") 
    "g c b" '(magit-branch-and-checkout :wk "Create branch and checkout")
    "g c c" '(magit-commit-create :wk "Create commit")
    "g c f" '(magit-commit-fixup :wk "Create fixup commit")
    "g C" '(magit-clone :wk "Clone repo")
    "g f" '(:ignore t :wk "Find") 
    "g f c" '(magit-show-commit :wk "Show commit")
    "g f f" '(magit-find-file :wk "Magit find file")
    "g f g" '(magit-find-git-config-file :wk "Find gitconfig file")
    "g F" '(magit-fetch :wk "Git fetch")
    "g g" '(magit-status :wk "Magit status")
    "g i" '(magit-init :wk "Initialize git repo")
    "g l" '(magit-log-buffer-file :wk "Magit buffer log")
    "g r" '(vc-revert :wk "Git revert file")
    "g s" '(magit-stage-file :wk "Git stage file")
    "g t" '(git-timemachine :wk "Git time machine")
    "g u" '(magit-stage-file :wk "Git unstage file"))

 (dt/leader-keys
    "h" '(:ignore t :wk "Help")
    "h a" '(counsel-apropos :wk "Apropos")
    "h b" '(describe-bindings :wk "Describe bindings")
    "h c" '(describe-char :wk "Describe character under cursor")
    "h d" '(:ignore t :wk "Emacs documentation")
    "h d a" '(about-emacs :wk "About Emacs")
    "h d d" '(view-emacs-debugging :wk "View Emacs debugging")
    "h d f" '(view-emacs-FAQ :wk "View Emacs FAQ")
    "h d m" '(info-emacs-manual :wk "The Emacs manual")
    "h d n" '(view-emacs-news :wk "View Emacs news")
    "h d o" '(describe-distribution :wk "How to obtain Emacs")
    "h d p" '(view-emacs-problems :wk "View Emacs problems")
    "h d t" '(view-emacs-todo :wk "View Emacs todo")
    "h d w" '(describe-no-warranty :wk "Describe no warranty")
    "h e" '(view-echo-area-messages :wk "View echo area messages")
    "h f" '(describe-function :wk "Describe function")
    "h F" '(describe-face :wk "Describe face")
    "h g" '(describe-gnu-project :wk "Describe GNU Project")
    "h i" '(info :wk "Info")
    "h I" '(describe-input-method :wk "Describe input method")
    "h k" '(describe-key :wk "Describe key")
    "h l" '(view-lossage :wk "Display recent keystrokes and the commands run")
    "h L" '(describe-language-environment :wk "Describe language environment")
    "h m" '(describe-mode :wk "Describe mode")
    "h r" '(:ignore t :wk "Reload")
    "h r r" '((lambda () (interactive)
                (load-file "~/.config/emacs/init.el")
                (ignore (elpaca-process-queues)))
              :wk "Reload emacs config")
    "h t" '(load-theme :wk "Load theme")
    "h v" '(describe-variable :wk "Describe variable")
    "h w" '(where-is :wk "Prints keybinding for command if set")
    "h x" '(describe-command :wk "Display full documentation for command"))

  (dt/leader-keys
    "m" '(:ignore t :wk "Org")
    "m a" '(org-agenda :wk "Org agenda")
    "m e" '(org-export-dispatch :wk "Org export dispatch")
    "m i" '(org-toggle-item :wk "Org toggle item")
    "m t" '(org-todo :wk "Org todo")
    "m B" '(org-babel-tangle :wk "Org babel tangle")
    "m T" '(org-todo-list :wk "Org todo list"))

  (dt/leader-keys
    "m b" '(:ignore t :wk "Tables")
    "m b -" '(org-table-insert-hline :wk "Insert hline in table"))

  (dt/leader-keys
    "m d" '(:ignore t :wk "Date/deadline")
    "m d t" '(org-time-stamp :wk "Org time stamp"))

  (dt/leader-keys
    "o" '(:ignore t :wk "Open")
    "o d" '(dashboard-open :wk "Dashboard")
    "o e" '(elfeed :wk "Elfeed RSS")
    "o f" '(make-frame :wk "Open buffer in new frame")
    "o F" '(select-frame-by-name :wk "Select frame by name"))

  ;; projectile-command-map already has a ton of bindings 
  ;; set for us, so no need to specify each individually.
  (dt/leader-keys
    "p" '(projectile-command-map :wk "Projectile"))

 (dt/leader-keys
    "q" '(:ignore t :wk "Quit")
    "q r" '(restart-emacs :wk "Restart emacs")
    "q q" '(kill-emacs :wk "Quit emacs"))
  
  (dt/leader-keys
    "r" '(:ignore t :wk "Radio")
    "r p" '(eradio-play :wk "Eradio play")
    "r s" '(eradio-stop :wk "Eradio stop")
    "r t" '(eradio-toggle :wk "Eradio toggle"))


  (dt/leader-keys
    "s" '(:ignore t :wk "Search")
    "s d" '(dictionary-search :wk "Search dictionary")
    "s m" '(man :wk "Man pages")
    "s o" '(pdf-occur :wk "Pdf search lines matching STRING")
    "s t" '(tldr :wk "Lookup TLDR docs for a command")
    "s w" '(woman :wk "Similar to man but doesn't require man"))

  (dt/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t e" '(eshell-toggle :wk "Toggle eshell")
    "t f" '(flycheck-mode :wk "Toggle flycheck")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t n" '(neotree-toggle :wk "Toggle neotree file viewer")
    "t o" '(org-mode :wk "Toggle org mode")
    "t r" '(rainbow-mode :wk "Toggle rainbow mode")
    "t t" '(visual-line-mode :wk "Toggle truncated lines")
    "t v" '(vterm-toggle :wk "Toggle vterm"))

  (dt/leader-keys
    "w" '(:ignore t :wk "Windows/Words")
    ;; Window splits
    "w c" '(evil-window-delete :wk "Close window")
    "w n" '(evil-window-new :wk "New window")
    "w s" '(evil-window-split :wk "Horizontal split window")
    "w v" '(evil-window-vsplit :wk "Vertical split window")
    ;; Window motions
    "w h" '(evil-window-left :wk "Window left")
    "w j" '(evil-window-down :wk "Window down")
    "w k" '(evil-window-up :wk "Window up")
    "w l" '(evil-window-right :wk "Window right")
    "w w" '(evil-window-next :wk "Goto next window")
    ;; Move Windows
    "w H" '(buf-move-left :wk "Buffer move left")
    "w J" '(buf-move-down :wk "Buffer move down")
    "w K" '(buf-move-up :wk "Buffer move up")
    "w L" '(buf-move-right :wk "Buffer move right")
    ;; Words
    "w d" '(downcase-word :wk "Downcase word")
    "w u" '(upcase-word :wk "Upcase word")
    "w =" '(count-words :wk "Count words/lines for buffer"))
)

(use-package git-timemachine
  :after git-timemachine
  :hook (evil-normalize-keymaps . git-timemachine-hook)
  :config
    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-j") 'git-timemachine-show-previous-revision)
    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-k") 'git-timemachine-show-next-revision)
)

(use-package magit)

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

;;----------------- C/Cpp Mode -------------------------------;;
;; 配置 corfu 作为轻量化补全框架
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)                ;; 循环选择补全项
  (corfu-auto t)                 ;; 自动弹出补全菜单
  (corfu-auto-delay 0.05)        ;; 自动补全延迟
  (corfu-auto-prefix 1)          ;; 自动补全触发的字符数
  :init
  (global-corfu-mode)            ;; 全局启用 corfu
  :config
  ;; 导航补全菜单的按键绑定
  (define-key corfu-map (kbd "TAB") 'corfu-next)
  (define-key corfu-map (kbd "<tab>") 'corfu-next)
  (define-key corfu-map (kbd "S-TAB") 'corfu-previous)
  (define-key corfu-map (kbd "<backtab>") 'corfu-previous))

;; 使用 orderless 提供更强大的模糊匹配
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic)) ;; 使用模糊匹配
  (completion-category-overrides '((file (styles . (partial-completion)))))) ;; 文件路径补全更友好

;; 禁用 org-mode 和 eshell-mode 的补全
(add-hook 'org-mode-hook (lambda () (corfu-mode -1)))
(add-hook 'eshell-mode-hook (lambda () (corfu-mode -1)))

;; 配置 lsp-mode
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (cmake-mode . lsp-deferred))
  :config
  (setq lsp-clients-clangd-args '("--header-insertion=never" "--clang-tidy" "--completion-style=detailed")
        lsp-enable-symbol-highlighting t
        lsp-enable-on-type-formatting t
        lsp-idle-delay 0.1
        lsp-diagnostics-provider :flycheck)
 (setq lsp-file-watch-ignored-directories '("[/\\\\]\\.git\\'" "[/\\\\]\\.hg\\'" "[/\\\\]\\.bzr\\'" "[/\\\\]_darcs\\'" "[/\\\\]\\.svn\\'" "[/\\\\]\\.idea\\'" "[/\\\\]\\.ensime_cache\\'" "[/\\\\]\\.eunit\\'" "[/\\\\]node_modules\\'" "[/\\\\]\\.fslckout\\'" "[/\\\\]\\.tox\\'" "[/\\\\]\\.stack-work\\'" "[/\\\\]\\.bloop\\'" "[/\\\\]\\.metals\\'" "[/\\\\]target\\'" "[/\\\\]\\.ccls-cache\\'")))

;; 配置 lsp-ui
(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-show-with-cursor t
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover t))

;; 安装和配置 cmake-mode
(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'" . cmake-mode)
  :mode ("\\.cmake\\'" . cmake-mode)
  :hook (cmake-mode . lsp-deferred)
  :config
  (setq cmake-tab-width 4))

;; general varible
(defvar my-cmake-build-dir "build"
  "The default build directory for CMake projects.")

(defvar my-cmake-compiler-list
  (if (eq system-type 'darwin)
      '("clang" "clang++")
    '("gcc" "g++" "clang" "clang++"))
  "A list of available compilers to choose from, adjusted for the system.")

;; general function
(defun my-locate-project-root ()
  "Locate the project root containing 'CMakeLists.txt'."
  (locate-dominating-file default-directory "CMakeLists.txt"))

(defun my-select-compiler ()
  "Select a compiler from `my-cmake-compiler-list`."
  (interactive)
  (let ((compiler (completing-read "Select Compiler: " my-cmake-compiler-list)))
    (setenv "CC" compiler)   ;; 设置 C 编译器
    (setenv "CXX" compiler) ;; 设置 C++ 编译器
    (message "Compiler set to: %s" compiler)))

(defun my-find-executables (build-dir)
  "Find executable files in BUILD-DIR."
  (let ((files (directory-files build-dir t "^[^\\.].*")))
    (seq-filter
     (lambda (file)
       (and (file-executable-p file) ;; 可执行文件
            (not (file-directory-p file)))) ;; 排除目录
     files)))

;; CMake 命令
(defun my-cmake-setup ()
  "Run CMake to configure the project."
  (interactive)
  (let ((project-root (my-locate-project-root)))
    (if project-root
        (let ((default-directory project-root))
          (compile (format "cmake -B %s -S . -DCMAKE_EXPORT_COMPILE_COMMANDS=ON" my-cmake-build-dir)))
      (message "No CMakeLists.txt found in the project!"))))

(defun my-cmake-build ()
  "Build the CMake project."
  (interactive)
  (let ((project-root (my-locate-project-root)))
    (if project-root
        (let ((default-directory project-root))
          (compile (format "cmake --build %s" my-cmake-build-dir)))
      (message "No CMakeLists.txt found in the project!"))))
(defun my-cmake-run ()
  "Run the compiled CMake project."
  (interactive)
  (let* ((project-root (my-locate-project-root))
         (build-dir (concat project-root my-cmake-build-dir))
         (executables (my-find-executables build-dir)))
    (if (and project-root executables)
        (let ((default-directory build-dir)
              (exe (completing-read "Select executable: " executables)))
          (if (eq system-type 'windows-nt)
              (compile (format "%s" (file-name-nondirectory exe))) ;; Windows 不需要 ./ 前缀
            (compile (format "./%s" (file-name-nondirectory exe))))) ;; Unix 使用 ./ 前缀
      (message "No executable found in the build directory!"))))

(defun my-cmake-debug ()
  "Debug the CMake project."
  (interactive)
  (let* ((project-root (my-locate-project-root))
         (build-dir (concat project-root my-cmake-build-dir))
         (executables (my-find-executables build-dir)))
    (if (and project-root executables)
        (let ((default-directory build-dir)
              (exe (completing-read "Select executable to debug: " executables)))
          (if (eq system-type 'windows-nt)
              (gdb (format "gdb -i=mi %s" (file-name-nondirectory exe))) ;; Windows 不需要 ./ 前缀
            (gdb (format "gdb -i=mi ./ %s" (file-name-nondirectory exe))))) ;; Unix 使用 ./ 前缀
      (message "No executable found in the build directory!"))))

;; CC Mode 配置

(global-set-key [escape] 'keyboard-escape-quit)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 35      ;; sets modeline height
        doom-modeline-bar-width 5    ;; sets right bar width
        doom-modeline-persp-name t   ;; adds perspective name to modeline
        doom-modeline-persp-icon t)) ;; adds folder icon next to persp name

(use-package neotree
  :config
  (setq neo-smart-open t
        neo-show-hidden-files t
        neo-window-width 35 
        neo-window-fixed-size nil
        inhibit-compacting-font-caches t
        projectile-switch-project-action 'neotree-projectile-action) 
        ;; truncate long file names in neotree
        (add-hook 'neo-after-create-hook
           #'(lambda (_)
               (with-current-buffer (get-buffer neo-buffer-name)
                 (setq truncate-lines t)
                 (setq word-wrap nil)
                 (make-local-variable 'auto-hscroll-mode)
                 (setq auto-hscroll-mode nil)))))

(setq org-agenda-files (list (concat mzn/agenda-path "/agenda.org")))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(eval-after-load 'org-indent '(diminish 'org-indent-mode))

;; 设置 Org-mode 标题的字体
;; 设置 Org-mode 全局字体为 Cantarell
(custom-set-faces
 '(org-default ((t (:family "Cantarell" :height 120 :weight regular))))
 '(org-level-1 ((t (:inherit org-default :height 1.3))))
 '(org-level-2 ((t (:inherit org-default :height 1.2))))
 '(org-level-3 ((t (:inherit org-default :height 1.1))))
 '(org-level-4 ((t (:inherit org-default :height 1.0))))
 '(org-level-5 ((t (:inherit org-default :height 1.0))))
 '(org-level-6 ((t (:inherit org-default :height 1.0))))
 '(org-level-7 ((t (:inherit org-default :height 1.0))))
 '(org-level-8 ((t (:inherit org-default :height 1.0))))
)

;; 设置 org-mode 字体的函数
(defun my-org-mode-setup ()
  (set-face-attribute 'default nil :family "Cantarell" :height 120)
  (set-face-attribute 'org-default nil :family "Cantarell" :height 120)
  (set-face-attribute 'org-level-1 nil :inherit 'org-default :height 1.3)
  (set-face-attribute 'org-level-2 nil :inherit 'org-default :height 1.2)
  (set-face-attribute 'org-level-3 nil :inherit 'org-default :height 1.1)
  (set-face-attribute 'org-level-4 nil :inherit 'org-default :height 1.0)
  (set-face-attribute 'org-level-5 nil :inherit 'org-default :height 1.0)
  (set-face-attribute 'org-level-6 nil :inherit 'org-default :height 1.0)
  (set-face-attribute 'org-level-7 nil :inherit 'org-default :height 1.0)
  (set-face-attribute 'org-level-8 nil :inherit 'org-default :height 1.0))

(add-hook 'org-mode-hook 'my-org-mode-setup)

(require 'org-tempo)

(defun org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . org-mode-visual-fill))

(setq org-src-preserve-indentation t)

(use-package toc-org
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))

(use-package pdf-tools
  :defer t
  :commands (pdf-loader-install)
  :mode "\\.pdf\\'"
  :bind (:map pdf-view-mode-map
              ("j" . pdf-view-next-line-or-next-page)
              ("k" . pdf-view-previous-line-or-previous-page)
              ("C-=" . pdf-view-enlarge)
              ("C--" . pdf-view-shrink))
  :init (pdf-loader-install)
  :config (add-to-list 'revert-without-query ".pdf"))

(add-hook 'pdf-view-mode-hook #'(lambda () (interactive) (display-line-numbers-mode -1)
                                                         (blink-cursor-mode -1)
                                                         (doom-modeline-mode -1)))

(use-package perspective
  :custom
  ;; NOTE! I have also set 'SCP =' to open the perspective menu.
  ;; I'm only setting the additional binding because setting it
  ;; helps suppress an annoying warning message.
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :init 
  (persp-mode)
  :config
  ;; Sets a file to write to when we save states
  (setq persp-state-default-file "~/.emacs.d/sessions"))

;; This will group buffers by persp-name in ibuffer.
(add-hook 'ibuffer-hook
          (lambda ()
            (persp-ibuffer-set-filter-groups)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))

;; Automatically save perspective states to file when Emacs exits.
(add-hook 'kill-emacs-hook #'persp-state-save)

(use-package projectile
  :config
  (projectile-mode 1))

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (clojure-mode . rainbow-delimiters-mode)))

(use-package rainbow-mode
  :diminish
  :hook org-mode prog-mode)

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
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package sudo-edit)

(use-package doom-themes
  :init (load-theme 'doom-dracula t))

(use-package tldr)

(add-to-list 'default-frame-alist '(alpha-background . 80)) ; For all new frames henceforth

(use-package transient)

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

(defun reader ()
  (interactive)
  (let ((choices '(("First"  . "Hi!")
                   ("Second" . 'second-choice)
                   ("Third"  . 'third-choice))))
    (alist-get
     (completing-read "Choose: " choices)
     choices nil nil 'message)))

(defun github-code-search ()
  "Search code on github for a given language."
  (interactive)
  (let ((language (completing-read
                   "Language: "
                   '("Emacs Lisp" "Python"  "Clojure" "R")))
        (code (read-string "Code: ")))
    (browse-url
     (concat "https://github.com/search?l=" language
             "&type=code&q=" code))))
  
(defun dm-search ()
  "Search various search engines."
  (interactive)
  (let ((engine (completing-read
                 "Search Engine: "
                 '("Arch Wiki" 
                   "Bing"
                   "Google"
                   "Wikipedia")))
        (query (read-string "Query: ")))
    (if (equal engine "Google")
      (browse-url
       (concat "https://www.google.com/search?q=" query)))))

(defun dt/key-value-completing (choice)                                     
  (interactive
   (list
    (let ((completions '(("1" "One") 
                         ("2" "Two")
                         ("3" "Three"))))              
      (cadr (assoc (completing-read "Choose: " completions) completions)))))
  (message "You choose `%s'" choice))
