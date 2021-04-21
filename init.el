;;; conf --- my emacs config
;;
;;; Commentary:
;; Inspirations:
;;  - https://bzg.fr/en/emacs-strip-tease.html/
;;
;;; Code:

;;; Install straight

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)


;;; Base packages

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
  :straight t
  :config
    (exec-path-from-shell-initialize)))

(use-package delight
  :straight t
  :config
  (delight-major-mode))

(use-package undo-tree
  :straight t
  :delight undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package autorevert
  :delight auto-revert-mode)

(progn
 (setq backup-directory-alist
       `(("." . ,(concat user-emacs-directory "backups")))))

(use-package evil
  :straight t
  :after undo-tree
  :init
  (setq evil-want-integration t)  ; needed for evil-collection
  (setq evil-want-keybinding nil) ; needed for evil-collection
  :config
  (evil-mode 1)
  (evil-define-key 'normal Info-mode-map (kbd "]") #'Info-forward-node)
  (evil-define-key 'normal Info-mode-map (kbd "[") #'Info-backward-node)
  (evil-define-key 'normal 'global "gt" 'counsel-semantic-or-imenu)
  (customize-set-variable 'evil-undo-system 'undo-tree))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init
   '(compile
     comint
     custom
     deadgrep
     debug
     dired
     ediff
     elfeed
     flycheck
     info
     log-edit
     log-view
	 magit
     mu4e
     occur
     simple
     which-key))
  (evil-collection-unimpaired-mode -1)
  (evil-collection-occur-setup))

(use-package flycheck
  :straight t
  :after nix-sandbox
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode)

  ;; from https://github.com/travisbhartwell/nix-emacs#flycheck
  (defun my/nix--flycheck-command-wrapper (command)
    (if-let ((sandbox (nix-current-sandbox)))
        (apply 'nix-shell-command sandbox command)
      command))
  (defun my/nix--flycheck-executable-find (cmd)
    (if-let ((sandbox (nix-current-sandbox)))
        (nix-executable-find sandbox cmd)
      (flycheck-default-executable-find cmd)))

  :config
  (setq flycheck-check-syntax-automatically '(save idle-change idle-buffer-switch)
        flycheck-relevant-error-other-file-show nil
        flycheck-command-wrapper-function 'my/nix--flycheck-command-wrapper
        flycheck-executable-find 'my/nix--flycheck-executable-find)
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.33))))

(use-package yasnippet
  :straight t
  :config
  (yas-global-mode))

(use-package flyspell
  :straight t
  :delight flyspell-mode
  :config
  ; Do not emit message when checking word
  (setq flyspell-issue-message-flag nil)

  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))
  (dolist (hook '(prog-mode-hook))
    (add-hook hook (lambda () (flyspell-prog-mode))))
  (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1))))

  (setq ispell-program-name "aspell")
  (setq ispell-list-command "--list")
  (flyspell-mode t))

(use-package company
  :straight t
  :delight company-mode
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 1)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-posframe
  :straight t
  :delight company-posframe
  :config
  (company-posframe-mode 1))

(use-package magit
  :straight t
  :bind (("C-x g g" . magit-status)
         ("C-x g d" . magit-diff-buffer-file)
         ("C-x g b" . magit-blame)
         ("C-x g l" . magit-log)
         ;; :map magit-file-mode-map
         ;; ("C-x g" . nil)
         :map magit-mode-map
         ("o" . magit-visit-thing)
         ("O" . magit-diff-visit-file-other-window))
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1
        magit-published-branches nil)
  ;; (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-assigned-pullreqs nil t)
  ;; (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-assigned-issues nil t)
  (evil-define-key 'normal magit-mode-map (kbd "o") 'magit-visit-thing)
  (evil-define-key 'normal magit-mode-map (kbd "O") 'magit-diff-visit-file-other-window))

(use-package forge
  :straight t
  :after magit
  :config
  (remove-hook 'magit-status-sections-hook 'forge-insert-pullreqs)
  (remove-hook 'magit-status-sections-hook 'forge-insert-issues))

(use-package auth-source)

(use-package auth-source-pass
  :straight t
  :config
  (auth-source-pass-enable))

(use-package ivy
  :straight t
  :delight ivy-mode
  :bind (("C-s" . 'swiper-thing-at-point)
         ("C-<return>" . 'ivy-immediate-done))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order)))
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package wgrep
  :straight t)

(use-package counsel
  :straight t
  :delight counsel-mode
  :after ivy
  :config
  (counsel-mode 1))

(use-package deadgrep
  :straight t
  :config
  (global-set-key (kbd "C-c g") #'deadgrep))

(use-package projectile
  :straight t
  :delight '(:eval (format " [%s]" (projectile-project-name)))
  :config
  (projectile-mode 1)
  (setq projectile-file-exists-local-cache-expire (* 5 60)))

(use-package counsel-projectile
  :straight t
  :config
  (counsel-projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package which-key
  :straight t
  :delight which-key-mode
  :config
  (which-key-mode 1))

(use-package eldoc
  :delight eldoc-mode)

(use-package eyebrowse
  :straight t
  :config
  (setq eyebrowse-new-workspace t)
  (eyebrowse-mode 1))

(use-package dired
  :config
  (setq dired-dwim-target 'dired-dwim-target-recent))

(use-package dired+
  :straight t)

;;; UI

(blink-cursor-mode 0)

;; Don't use messages that you don't read
(setq initial-scratch-message ""
      inhibit-startup-message t)

(setq visible-bell t
      ring-bell-function 'ignore)

(customize-set-variable 'inhibit-startup-echo-area-message "ibizaman")

(toggle-frame-fullscreen)

(menu-bar-mode 0)
(tool-bar-mode 0)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))

(when (memq window-system '(mac ns))
  (if (x-list-fonts "*-Inconsolata-*")
      (set-face-attribute 'default nil :font "Inconsolata-13")
    (set-face-attribute 'default nil :font "InconsolataG-10")))

(setq kill-do-not-save-duplicates t
      ; From https://stackoverflow.com/a/29092845/1013628
      select-enable-clipboard t)

(setq-default tab-width 4)
(defun my/disable-tabs ()
  "Disable tabs and set them to 4 spaces."
  (setq-default tab-width 4)
  (setq tab-width 4)
  (setq indent-tabs-mode nil))
;; Tabs are used to format buffer with `lsp-format-buffer'.
(add-hook 'haskell-mode-hook 'my/disable-tabs)

;;;; Modeline

(column-number-mode 1)

;; Hide modeline
;; See http://bzg.fr/emacs-hide-mode-line.html
(progn
  (defvar-local hidden-mode-line-mode nil)
  (defvar-local hide-mode-line nil)

  (define-minor-mode hidden-mode-line-mode
    "Minor mode to hide the mode-line in the current buffer."
    :init-value nil
    :global nil
    :variable hidden-mode-line-mode
    :group 'editing-basics
    (if hidden-mode-line-mode
        (setq hide-mode-line mode-line-format
              mode-line-format nil)
      (setq mode-line-format hide-mode-line
            hide-mode-line nil))
    (force-mode-line-update)
    ;; Apparently force-mode-line-update is not always enough to
    ;; redisplay the mode-line
    (redraw-display)
    (when (and (called-interactively-p 'interactive)
               hidden-mode-line-mode)
      (run-with-idle-timer
       0 nil 'message
       (concat "Hidden Mode Line Mode enabled.  "
               "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

  ;; Activate hidden-mode-line-mode
  (hidden-mode-line-mode 1)
  (add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)

  ;; Command to toggle the display of the mode-line as a header
  (defvar-local header-line-format nil)
  (defun mode-line-in-header ()
    (interactive)
	(if (not header-line-format)
		(setq header-line-format (if hidden-mode-line-mode hide-mode-line mode-line-format)
			  mode-line-format nil)
	  (if hidden-mode-line-mode
		  (setq hide-mode-line header-line-format)
		(setq mode-line-format header-line-format))
	  (setq header-line-format nil)
	(set-window-buffer nil (current-buffer)))
  (global-set-key (kbd "C-s-SPC") 'mode-line-in-header)))

;;;; Theme

(use-package mustang-theme
  :straight t
  :config
  (custom-theme-set-faces
   'mustang
   '(company-scrollbar-bg ((t (:background "#6080a0"))))
   '(company-scrollbar-fg ((t (:background "#7090ff"))))
   '(company-tooltip ((t (:background "#2e5077"))))
   '(company-tooltip-annotation ((t nil)))
   '(company-tooltip-common ((t (:foreground "#d73c3f"))))
   '(company-tooltip-common-selection ((t (:foreground "#921e20"))))
   '(company-tooltip-selection ((t (:background "#ff9642" :foreground "#202020"))))
   '(diff-added ((t (:inherit ediff-even-diff-B))))
   '(diff-changed ((t (:inherit ediff-even-diff-C))))
   '(diff-refine-added ((t (:inherit ediff-fine-diff-B))))
   '(diff-refine-changed ((t (:inherit ediff-fine-diff-C))))
   '(diff-refine-removed ((t (:inherit ediff-fine-diff-A))))
   '(diff-removed ((t (:inherit ediff-even-diff-A))))
   '(ediff-current-diff-C ((t (:background "#554433"))))
   '(ediff-even-diff-A ((t (:background "#553333"))))
   '(ediff-even-diff-Ancestor ((t (:background "#004151"))))
   '(ediff-even-diff-B ((t (:background "#335533"))))
   '(ediff-even-diff-C ((t (:background "#5a442e"))))
   '(ediff-fine-diff-A ((t (:background "#46150f"))))
   '(ediff-fine-diff-B ((t (:background "#0f4615"))))
   '(ediff-fine-diff-C ((t (:background "#46310f"))))
   '(ediff-odd-diff-A ((t (:background "#553333"))))
   '(ediff-odd-diff-Ancestor ((t (:background "#004151"))))
   '(ediff-odd-diff-B ((t (:background "#335533"))))
   '(ediff-odd-diff-C ((t (:background "#5a442e"))))
   '(eldoc-highlight-function-argument ((t (:inherit bold :underline t))))
   '(eyebrowse-mode-line-active ((t (:inherit mode-line-emphasis :inverse-video t))))
   '(font-lock-warning-face ((t (:background "#202020" :foreground "#ff6523"))))
   '(font-lock-comment-face ((t (:foreground "RoyalBlue1"))))
   '(font-lock-comment-delimiter-face ((t (:foreground "RoyalBlue1"))))
   '(git-gutter+-added ((t (:inherit font-lock-comment-face :weight bold))))
   '(git-gutter+-deleted ((t (:inherit font-lock-comment-face :weight bold))))
   '(git-gutter+-modified ((t (:inherit font-lock-comment-face :weight bold))))
   '(magit-diff-context-highlight ((t (:background "#312C33" :foreground "grey70"))))
   '(magit-diff-file-heading ((t (:background "#794214" :weight bold))))
   '(magit-diff-file-heading-highlight ((t (:inherit magit-section-highlight :background "#a55a1c"))))
   '(magit-diff-hunk-heading ((t (:background "#113f67" :foreground "grey90" :weight bold))))
   '(magit-diff-hunk-heading-highlight ((t (:background "#38598b"))))
   '(mu4e-view-body-face ((t (:background "gray10"))))
   '(org-block-begin-line ((t (:inherit org-meta-line :background "#102D3F" :foreground "#3A86B4" :slant normal))))
   '(org-meta-line ((t (:inherit font-lock-comment-face :slant italic))))
   '(vertical-border ((t (:foreground "gray23"))))))
(load-theme 'mustang t)


;;; Miscellaneous

;; Use only one space after a dot
(setq sentence-end-double-space nil)

;; Delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Save some variables with savehist
(require 'savehist)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(savehist-mode t)

;; When saving a file that starts with `#!’, make it executable
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Prompt to save customization before quitting
(add-hook 'kill-emacs-query-functions
      'custom-prompt-customize-unsaved-options)

;; Add keybindings to process-menu-mode
(progn
  (defun ibizaman/process-menu-add-bindings()
    "Add bindings to process-menu-mode."
    (define-key process-menu-mode-map (kbd "g r") 'list-processes)
    (define-key process-menu-mode-map (kbd "x") 'process-menu-delete-process)
    (evil-define-key 'normal process-menu-mode-map (kbd "x") 'process-menu-delete-process))

  (add-hook 'process-menu-mode-hook 'ibizaman/process-menu-add-bindings))

;; Show ascii characters in compile mode
(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))


;;; Helper functions
(defun get-secret (host user)
  (if-let* ((item (nth 0 (auth-source-search :host host :user user :max 1))))
      (funcall (plist-get item :secret))
    (error (format "Could not find secret for host:%s user:%s" host user))))

;; Shortcuts for jumping around functions
(progn
  (defun ibizaman/jump-functions()
    "Add bindings to move around functions."
    (define-key prog-mode-map (kbd "C-k") 'beginning-of-defun)
    (define-key prog-mode-map (kbd "C-j") 'end-of-defun))

  (add-hook 'prog-mode-hook 'ibizaman/jump-functions))


;;; Helper packages

(use-package expand-region
  :straight t
  :bind (("C-c =" . (lambda () (interactive) (er--expand-region-1)))
     ("C-c -" . (lambda () (interactive) (er/contract-region 1)))))

(defun use-region-or-expand-region ()
  "Use region if active or expand region at point."
  (when (not (use-region-p))
    (let ((inhibit-message t))
      (call-interactively 'er/expand-region))))

(use-package helpful
  :straight t
  :bind (("C-h f" . #'helpful-callable)
         ("C-h v" . #'helpful-variable)
         ("C-h k" . #'helpful-key))
  :config
  (global-set-key (kbd "C-h V") #'customize-variable)
  (global-set-key (kbd "C-h G") #'customize-group))


(use-package ediff
  :config
  (setq ediff-merge-split-window-function 'split-window-horizontally
    ediff-split-window-function 'split-window-horizontally
    ediff-window-setup-function 'ediff-setup-windows-plain))


(use-package ibuffer
  :straight t
  :bind (("C-x C-b" . 'ibuffer)))


(use-package git-link
  :straight t
  :config
  (defun git-link-master-branch ()
    (interactive)
    (let ((git-link-default-branch "master"))
      (call-interactively 'git-link)))
  (defun git-link-at-commit ()
    (interactive)
    (let ((git-link-use-commit t))
      (call-interactively 'git-link)))
  (defun git-link-master-branch-at-commit ()
    (interactive)
    (let ((git-link-default-branch "master")
      (git-link-use-commit t))
      (call-interactively 'git-link))))


(use-package emacs-org-babel-conf
  :straight (emacs-org-babel-conf :type git :host github :repo "ibizaman/emacs-org-babel-conf" :branch "master"))


(use-package hydra
  :straight t)


(use-package hledger-mode
  :straight (hledger-mode :type git :host github :repo "ibizaman/hledger-mode" :branch "master")
  :config
  (setq hledger-jfile "~/org/hledger.journal")
  (add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))
  (add-to-list 'company-backends 'hledger-company)

  (evil-define-key 'normal hledger-mode-map (kbd "B") 'hledger-balancesheet*)
  (evil-define-key 'normal hledger-mode-map (kbd "D") 'hledger-reschedule)
  (evil-define-key 'normal hledger-view-mode-map (kbd "d") 'hledger-report-at-day)
  (evil-define-key 'normal hledger-view-mode-map (kbd "n") 'hledger-next-report)
  (evil-define-key 'normal hledger-view-mode-map (kbd "p") 'hledger-prev-report))

(use-package csv-mode
  :straight t)


;;; Org

(use-package org
  :straight t
  :after evil
  :init
  (defun my/org-mode-hook-evil ()
      (setq evil-auto-indent nil))
  (add-hook 'org-mode-hook 'my/org-mode-hook-evil)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (sql . t)
     (python . t)
     (shell . t)
     (dot . t)
     (eshell . t)))
   (setq org-link-use-indirect-buffer-for-internals t)

  (org-babel-lob-ingest "~/.emacs.d/emacs-lob.org")

  (defun ibizaman/org-copy-element ()
    (interactive)
    (let* ((elem (org-element-at-point))
           (beg (org-element-property :begin elem))
           (end (org-element-property :end elem)))
      (copy-region-as-kill beg end)
      (goto-char end)))

  (defun ibizaman/org-babel-goto-tangle-file ()
    (if-let* ((args (nth 2 (org-babel-get-src-block-info t)))
              (tangle (or (alist-get :dir args) (alist-get :tangle args))))
        (when (not (equal "no" tangle))
          (find-file-other-window tangle)
          t)))
  (add-hook 'org-open-at-point-functions 'ibizaman/org-babel-goto-tangle-file)

  (setq org-log-done 'time
        org-babel-hash-show-time t
        org-adapt-indentation nil
        org-edit-src-content-indentation 0
        org-babel-uppercase-example-markers t
        org-src-tab-acts-natively t)

  (add-hook 'org-capture-prepare-finalize-hook 'org-id-store-link)

  (evil-define-key 'normal org-mode-map (kbd "<tab>") 'org-cycle)
  :bind (("C-c j" . outline-next-heading)
         ("C-c k" . outline-previous-heading)
         ("C-c h" . outline-up-heading)
         ("C-c l" . outline-show-subtree)
         ("C-c c" . org-capture)
         ("C-c C-l" . org-store-link)
         :map org-mode-map
         ("C-c o d" . org-cut-element)
         ("C-c o c" . ibizaman/org-copy-element)
         ("<tab>" . org-cycle)))


(use-package ob-async
  :straight t
  :after org)

(use-package ob-python
  :after org)

(use-package ob-shell
  :after org)

(use-package ob-tmux
  :straight (ob-tmux :type git :host nil :repo "https://github.com/ahendriksen/ob-tmux.git")
  :config
  (setq org-babel-default-header-args:tmux
        '((:results . "silent")))
  (setq org-babel-tmux-terminal "iterm"
        org-babel-tmux-session-prefix "ob-"))

(use-package ox-pandoc
  :disabled
  :straight (ox-pandoc :type git :host github :repo "mgcyung/ox-pandoc" :branch "master"))

(use-package org-pandoc-import
  :disabled
  :straight (:host github
             :repo "tecosaur/org-pandoc-import"
             :files ("*.el" "filters" "preprocessors"))
  :after ox-pandoc
  :config
  (org-pandoc-import-transient-mode 1))

;;; Mu4e
(if (memq window-system '(mac ns))
    (add-to-list 'load-path "/usr/local/Cellar/mu/1.0/share/emacs/site-lisp/mu/mu4e")
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e"))

(use-package mu4e
  :if (locate-library "mu4e")
  :config
  (require 'mu4e-contrib)

  (progn
    (defcustom ibizaman/mu4e-unread-excluded-lists nil
      "Mailing lists to be excluded from default unread view."
      :group 'mu4e
      :type '(repeat string))

    (defun ibizaman/mu4e-add-message-list-to-excluded-lists (msg)
      (let ((list (mu4e-message-field msg :mailing-list)))
        (add-to-list 'ibizaman/mu4e-unread-excluded-lists list)
        (message "Added %s to excluded list" list)))

    (add-to-list 'mu4e-headers-actions
                 '("Exclude list" . ibizaman/mu4e-add-message-list-to-excluded-lists) t)

    (defun ibizaman/mu4e-generate-unread-filter ()
      (concat "flag:unread "
              "AND NOT flag:trashed "
              "AND NOT maildir:/Gmail/recruiting "
              "AND NOT maildir:\"/Gmail/[Google Mail].Trash\" "
              "AND NOT maildir:\"/Gmail/[Google Mail].Spam\" "
              (mapconcat (lambda (v) (concat " AND NOT list:" v))
                         ibizaman/mu4e-unread-excluded-lists "")))

    (defun ibizaman/mu4e-get-unread-list-filter-query (wanted-list)
      (interactive (list (completing-read "List: " ibizaman/mu4e-unread-excluded-lists)))
      (concat "flag:unread AND NOT flag:trashed AND list:" wanted-list)))

  (setq mail-user-agent                  'mu4e-user-agent
        mu4e-maildir                     "~/Maildir"
        mu4e-use-fancy-chars             t
        mu4e-attachment-dir              "~/Maildir/Attachments/Gmail"
        mu4e-view-show-images            t
        mu4e-confirm-quit                nil
        mu4e-completing-read-function    'ivy-completing-read
        mu4e-hide-index-messages         t
        message-kill-buffer-on-exit      t
        mu4e-html2text-command           'mu4e-shr2text
        shr-color-visible-luminance-min  80  ; for dark themes
        shr-color-visible-distance-min   5
        mu4e-refile-folder               "/Gmail/[Google Mail].All Mail")

  (defun ibizaman/mu4e-set-contexts ()
    (setq mu4e-contexts
          `( ,(make-mu4e-context
               :name "Private"
               :enter-func (lambda () (mu4e-message "Entering Private context"))
               :leave-func (lambda () (mu4e-message "Leaving Private context"))
               ;; we match based on the contact-fields of the message
               :match-func (lambda (msg)
                             (when msg
                               (string-match-p "^/Gmail" (mu4e-message-field msg :maildir))))
               :vars `( ( user-mail-address      . "ibizapeanut@gmail.com"  )
                        ( user-full-name         . "Pierre Penninckx" )
                        ( mu4e-drafts-folder     . "/Gmail/[Google Mail].Drafts" )
                        ( mu4e-sent-folder       . "/Gmail/[Google Mail].Sent Mail" )
                        ( mu4e-trash-folder      . "/Gmail/[Google Mail].Trash" )
                        ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
                        ( mu4e-sent-messages-behavior . delete )
                        ( mu4e-maildir-shortcuts .
                                                 ( ("/Gmail/INBOX"                     . ?i)
                                                   ("/Gmail/recruiting"                . ?r)
                                                   ("/Gmail/[Google Mail].Sent Mail"   . ?s)
                                                   ("/Gmail/[Google Mail].Trash"       . ?t)
                                                   ("/Gmail/[Google Mail].All Mail"    . ?a)) )
                        ( mu4e-get-mail-command . "offlineimap" )
                        ( mu4e-bookmarks .
                                         (,(make-mu4e-bookmark
                                            :name  "Unread messages not list"
                                            :query (lambda () (ibizaman/mu4e-generate-unread-filter))
                                            :key ?u)
                                          ,(make-mu4e-bookmark
                                            :name  "Recruiting"
                                            :query "maildir:/Gmail/recruiting"
                                            :key ?r)
                                          ,(make-mu4e-bookmark
                                            :name  "Unread messages all"
                                            :query (concat "flag:unread "
                                                           "AND NOT flag:trashed"
                                                           "AND NOT maildir:\"/Gmail/[Google Mail].Trash\" "
                                                           "AND NOT maildir:\"/Gmail/[Google Mail].Spam\" ")
                                            :key ?i)
                                          ,(make-mu4e-bookmark
                                            :name  "Unread list messages"
                                            :query (lambda () (call-interactively 'ibizaman/mu4e-get-unread-list-filter-query))
                                            :key ?l)
                                          ,(make-mu4e-bookmark
                                            :name "Today's messages"
                                            :query "date:today..now"
                                            :key ?t)
                                          ,(make-mu4e-bookmark
                                            :name "Last 7 days"
                                            :query "date:7d..now AND NOT flag:list AND NOT maildir:/Gmail/recruiting"
                                            :key ?w)
                                          ,(make-mu4e-bookmark
                                            :name "Messages with images"
                                            :query "mime:image/*"
                                            :key ?p)
                                          ,(make-mu4e-bookmark
                                            :name "Drafts"
                                            :query "flag:draft"
                                            :key ?d))))))))
  (ibizaman/mu4e-set-contexts)

  (require 'smtpmail)
  (setq message-send-mail-function 'smtpmail-send-it
        user-mail-address "ibizapeanut@gmail.com"
        starttls-use-gnutls t
        starttls-gnutls-program "gnutls-cli"
        starttls-extra-arguments nil
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-debug-info t
        smtpmail-smtp-user "ibizapeanut@gmail.com"))

(use-package org-mu4e
  :after org mu4e)

(use-package mu4e-maildirs-extension
  :straight t
  :after mu4e
  :config
  (mu4e-maildirs-extension))


;;; Language specific packages

(global-set-key (kbd "C-c C-j") 'find-function)

;;;; Language Server Protocol

(defun my/lsp-format-buffer-silent ()
  "Silence errors from `lsp-format-buffer'."
  (ignore-errors (lsp-format-buffer)))

(use-package lsp-mode
  :straight t
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-x l")
  :hook ((sh-mode . lsp-deferred)
         (javascript-mode . lsp-deferred)
         (html-mode . lsp-deferred)
         (before-save . my/lsp-format-buffer-silent)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-signature-auto-activate t)
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq gc-cons-threshold 100000000
        read-process-output-max (* 1024 1024)) ;; 1mb
  (lsp-lens-mode t))

(use-package lsp-ui
  :straight t
  :hook (lsp-mode-hook . lsp-ui-mode)
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-flycheck-enable t
        lsp-ui-flycheck-live-reporting nil))

(use-package company-lsp
  :straight t
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))

;;;; Elisp

(defun eval-point-region-and-deactivate ()
  "Evaluate region or expanded region and deactivates region when done."
  (interactive)
  (use-region-or-expand-region)
  (condition-case-unless-debug err
      (call-interactively 'eval-region)
    (error (deactivate-mark)
           (signal (car err) (cdr err))))
  (deactivate-mark))


(use-package elisp-mode
  :bind (("C-c C-c" . eval-point-region-and-deactivate)))

;;;; Haskell

(use-package nix-sandbox
  :straight t)

(use-package haskell-mode
  :straight t
  :after nix-sandbox
  :init
  (defun my/haskell-set-stylish ()
    (if-let* ((sandbox (nix-current-sandbox))
              (fullcmd (nix-shell-command sandbox "brittany"))
              (path (car fullcmd))
              (args (cdr fullcmd)))
      (setq-local haskell-mode-stylish-haskell-path path
                  haskell-mode-stylish-haskell-args args)))
  (defun my/haskell-set-hoogle ()
    (if-let* ((sandbox (nix-current-sandbox)))
        (setq-local haskell-hoogle-command (nix-shell-string sandbox "hoogle"))))
  :hook ((haskell-mode . capitalized-words-mode)
         (haskell-mode . haskell-decl-scan-mode)
         (haskell-mode . haskell-indent-mode)
         (haskell-mode . haskell-indentation-mode)
         (haskell-mode . my/haskell-set-stylish)
         (haskell-mode . my/haskell-set-hoogle)
         (haskell-mode . lsp-deferred)
         (haskell-mode . haskell-auto-insert-module-template))
  :config
  (defun my/haskell-hoogle--server-command (port)
    (if-let* ((hooglecmd `("hoogle" "serve" "--local" "-p" ,(number-to-string port)))
              (sandbox (nix-current-sandbox)))
        (apply 'nix-shell-command sandbox hooglecmd)
      hooglecmd))
  (setq haskell-hoogle-server-command 'my/haskell-hoogle--server-command
        haskell-stylish-on-save t))

(use-package lsp-haskell
  :straight t
  :after nix-sandbox
  :init
  (setq lsp-prefer-flymake nil)
  (require 'lsp-haskell)
  :config
  ;; from https://github.com/travisbhartwell/nix-emacs#haskell-mode
  (defun my/nix--lsp-haskell-wrapper (args)
    (if-let ((sandbox (nix-current-sandbox)))
        (apply 'nix-shell-command sandbox args)
      args))
  (setq lsp-haskell-server-path "haskell-language-server-wrapper"
        lsp-haskell-server-wrapper-function 'my/nix--lsp-haskell-wrapper))

;;;; Nix

(use-package nix-mode
  :straight t
  :mode "\\.nix\\'"
  :init
  (require 'nix-build))

;;;; Markdown

(use-package markdown-mode
  :straight t
  :commands (markdown-mode gf-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;;; Yaml

(use-package yaml-mode
  :straight t
  :hook (yaml-mode . lsp-deferred))

;;;; Go

(use-package go-mode
  :straight t
  :hook (go-mode . lsp-deferred)
  :bind (([remap find-function] . 'lsp-find-definition))
  :config
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

(use-package gotest
  :straight t
  :after company go-mode)

(use-package go-dlv
  :straight t
  :after gotest)

(use-package ob-go
  :straight t)

(eval-after-load 'hydra
  (eval-after-load 'go-mode
     '(progn
        (defgroup go-test-args nil
          "Go test args specific customization."
          :group 'go-test)


        (defcustom go-test-args-base-tags '("unit")
          "Available tags that can be toggled."
          :group 'go-test-args
          :type '(repeat string))

        (defvar go-test-args-tags nil
          "Go test arg --tags=")

        (defun go-test-args-tags-toggle ()
          "Toggle tags argument."
          (go-test-args--toggle-list 'go-test-args-tags 'go-test-args-base-tags))

        (defun go-test-args-tags-arg ()
          "Get tags argument."
          (if (null go-test-args-tags) ""
            (format "--tags=\"%s\"" (mapconcat 'identity (symbol-value 'go-test-args-tags) " "))))


        (defcustom go-test-args-base-cpu '("1" "2" "3" "4" "5" "6" "7" "8")
          "Available tags that can be toggled."
          :group 'go-test-args
          :type '(repeat string))

        (defvar go-test-args-cpu nil
          "Go test arg --cpu=")

        (defun go-test-args-cpu-toggle ()
          "Toggle cpu argument."
          (go-test-args--toggle-list 'go-test-args-cpu 'go-test-args-base-cpu))

        (defun go-test-args-cpu-arg ()
          "Get cpu argument."
          (if (null go-test-args-cpu) ""
            (format "--cpu=%s" (mapconcat 'identity (symbol-value 'go-test-args-cpu) ","))))


        (defvar go-test-args-verbose nil
          "Go test arg -v")

        (defun go-test-args-verbose-toggle ()
          "Toggle verbose argument."
          (go-test-args--toggle-bool 'go-test-args-verbose))

        (defun go-test-args-verbose-arg ()
          "Get verbose argument."
          (if go-test-args-verbose "-v" ""))


        (defvar go-test-args-race nil
          "Go test arg --race")

        (defun go-test-args-race-toggle ()
          "Toggle race argument."
          (go-test-args--toggle-bool 'go-test-args-race))

        (defun go-test-args-race-arg ()
          "Get race argument."
          (if go-test-args-race "--race" ""))


        (defvar go-test-args-count nil
          "Go test arg --count")

        (defun go-test-args-count-toggle ()
          "Toggle count argument."
          (go-test-args--toggle-number 'go-test-args-count))

        (defun go-test-args-count-arg ()
          "Get count argument."
          (if (null go-test-args-count) ""
            (format "--count=\"%s\"" go-test-args-count)))


        (defvar go-test-args-parallel nil
          "Go test arg --parallel")

        (defun go-test-args-parallel-toggle ()
          "Toggle parallel argument."
          (go-test-args--toggle-number 'go-test-args-parallel))

        (defun go-test-args-parallel-arg ()
          "Get parallel argument."
          (if (null go-test-args-parallel) ""
            (format "--parallel=\"%s\"" go-test-args-parallel)))


        (defvar go-test-args-coverprofile nil
          "Go test arg --coverprofile")

        (defun go-test-args-coverprofile-toggle ()
          "Toggle coverprofile argument."
          (go-test-args--toggle-str 'go-test-args-coverprofile))

        (defun go-test-args-coverprofile-arg ()
          "Get coverprofile argument."
          (if (null go-test-args-coverprofile) ""
            (format "--coverprofile=\"%s\"" go-test-args-coverprofile)))


        (defvar go-test-args-coverpkg nil
          "Go test arg --coverpkg=")

        (defun go-test-args-coverpkg-toggle ()
          "Toggle coverpkg argument."
          (go-test-args--toggle-list 'go-test-args-coverpkg ()))

        (defun go-test-args-coverpkg-arg ()
          "Get coverpkg argument."
          (if (null go-test-args-coverpkg) ""
            (format "--coverpkg=\"%s\"" (mapconcat 'identity (symbol-value 'go-test-args-coverpkg) ","))))


        (defvar go-test-args-custom nil
          "Custom test arguments")

        (defun go-test-args-custom-toggle ()
          "Toggle custom test argument."
          (go-test-args--toggle-str 'go-test-args-custom))

        (defun go-test-args-custom-arg ()
          "Get custom test argument."
          (if (null go-test-args-custom) ""
            (symbol-value 'go-test-args-custom)))


        (defun go-test-args--set-global-var ()
          "Set go-test-args from variables toggled in this module."
          (let* ((allvalues `(,(go-test-args-tags-arg)
                              ,(go-test-args-count-arg)
                              ,(go-test-args-parallel-arg)
                              ,(go-test-args-verbose-arg)
                              ,(go-test-args-race-arg)
                              ,(go-test-args-cpu-arg)
                              ,(go-test-args-coverprofile-arg)
                              ,(go-test-args-coverpkg-arg)
                              ,(go-test-args-custom-arg)))
                 (nonnil (seq-filter (lambda (elem) (not (or (null elem) (equal elem ""))))
                                     allvalues)))
            (setq go-test-args (mapconcat 'identity nonnil " "))))


        (defun go-test-args--toggle-in-list (list element)
          "Toggles an ELEMENT in a LIST."
          (if (member element (symbol-value list))
              (set list (delete element (symbol-value list)))
            (sort (add-to-list list element) 'string<)))

        (defun go-test-args--toggle-list (var base-var)
          "Toggle an element from BASE-VAR in VAR."
          (let* ((toggle-list (sort (delete-dups (append (symbol-value base-var) (copy-sequence (symbol-value var)))) 'string<))
                 (element (completing-read "Pick an element to toggle: " toggle-list)))
            (go-test-args--toggle-in-list var element))
          (go-test-args--set-global-var))

        (defun go-test-args--toggle-bool (var)
          (if (symbol-value var)
              (set var nil)
            (set var t))
          (go-test-args--set-global-var))

        (defun go-test-args--toggle-number (var)
          (let ((number (read-string "Enter a number (leave empty to unset the argument): ")))
            (if (equal number "")
                (set var nil)
              (set var number)))
          (go-test-args--set-global-var))

        (defun go-test-args--toggle-str (var)
          (let ((value (read-string "Enter a value (leave empty to unset the argument): ")))
            (if (equal value "")
                (set var nil)
              (set var value)))
          (go-test-args--set-global-var))

        (defhydra go-test-args-hydra (:hint nil)
          "
_T_ags:            %`go-test-args-tags
_C_ount:           %`go-test-args-count
_P_arallel:        %`go-test-args-parallel
_v_erbose:         %`go-test-args-verbose
_r_ace:            %`go-test-args-race
cp_u_:             %`go-test-args-cpu
c_o_verprofile:    %`go-test-args-coverprofile
coverp_k_g:        %`go-test-args-coverpkg
_c_ustom:          %`go-test-args-custom

go-test-args: %s(symbol-value 'go-test-args)

_t_: run current _t_est   _b_: run current _b_enchmark   _O_: show coverage
_f_:      ... in _f_ile   _B_:           ^^... in file
_p_:   ... in _p_ackage   _N_:        ^^... in package
"
          ("T" (go-test-args-tags-toggle))
          ("C" (go-test-args-count-toggle))
          ("P" (go-test-args-parallel-toggle))
          ("v" (go-test-args-verbose-toggle))
          ("r" (go-test-args-race-toggle))
          ("u" (go-test-args-cpu-toggle))
          ("o" (go-test-args-coverprofile-toggle))
          ("k" (go-test-args-coverpkg-toggle))
          ("c" (go-test-args-custom-toggle))

          ("t" (go-test-current-test) :color blue)
          ("f" (go-test-current-file) :color blue)
          ("p" (go-test-current-project) :color blue)
          ("b" (go-test-current-benchmark) :color blue)
          ("B" (go-test-current-file-benchmarks) :color blue)
          ("N" (go-test-current-project-benchmarks) :color blue)
          ("O" (go-coverage) :color blue))

        (define-key go-mode-map (kbd "C-c t") 'go-test-args-hydra/body))))


;;;; Arduino

(use-package arduino-mode
  :straight t)

;;; Elfeed

(use-package elfeed
  :straight t)

(use-package elfeed-protocol
  :straight t

  :config
  (setq elfeed-use-curl t
        elfeed-protocol-ttrss-maxsize 200
        elfeed-feeds  `(("ttrss+https://admin@tt-rss.tiserbox.com"
                         :use-authinfo t)))
  (elfeed-set-timeout 36000)
  (elfeed-protocol-enable))


;;; Customization

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(link ((t (:foreground "#b2d8e8" :underline nil))))
 '(ediff-even-diff-A ((t (:inherit ediff-current-diff-A))))
 '(ediff-even-diff-Ancestor ((t (:inherit ediff-current-diff-Ancestor))))
 '(link-visited ((t (:inherit link))))
 '(org-block ((t (:inherit default :foreground "#e2e2e5"))))
 '(org-level-1 ((t (:foreground "#df9f2d" :underline t :weight bold :height 1.3))))
 '(org-level-2 ((t (:foreground "#df9f2d" :underline t :weight normal :height 1.3))))
 '(org-level-3 ((t (:foreground "#b1d631" :underline t :weight bold :height 1.3))))
 '(org-level-4 ((t (:foreground "#b1d631" :underline t :weight normal :height 1.3))))
 '(org-level-5 ((t (:foreground "#b1d631" :height 1.1))))
 '(org-level-6 ((t (:foreground "#5091a1" :underline t :weight bold :height 1.1))))
 '(org-level-7 ((t (:foreground "#5091a1" :underline t :height 1.1))))
 '(org-level-8 ((t (:foreground "#5091a1" :height 1.1))))
 '(org-link ((t (:foreground unspecified :underline nil :inherit link))))
 '(org-meta-line ((t (:inherit font-lock-comment-face :extend t :background "#303b4b" :foreground "#b0e2ff")))))

;;; init.el ends here
