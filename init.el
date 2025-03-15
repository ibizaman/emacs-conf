;;; conf --- my emacs config
;;
;;; Commentary:
;; Inspirations:
;;  - https://bzg.fr/en/emacs-strip-tease.html/
;;
;;; Code:

;; See https://github.com/jwiegley/use-package/issues/436
(require 'use-package)

;;; Org

(use-package org
  :ensure t
  :init
  (defun org-clocking-buffer ())
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
        org-startup-indented t
		org-src-preserve-indentation t
        org-src-tab-acts-natively t)

  (setq org-todo-keywords
		'((sequence "TODO(t)" "INPROGRESS(i!)" "BLOCKED(b@!/@!)" "|" "DONE(d@!)" "CANCELLED(c@!)")))

  (defface org-todo-inprogress
	`((t (:inherit org-warning)))
	"Face for in progress todo keywords"
	:group 'org-faces)

  (defface org-todo-blocked
	`((t (:inherit org-warning)))
	"Face for blocked todo keywords"
	:group 'org-faces)

  (defface org-todo-ready
	`((t (:inherit org-warning)))
	"Face for ready to start todo keywords"
	:group 'org-faces)

  (setq org-todo-keyword-faces
		'(("INPROGRESS" . org-todo-inprogress)
		  ("IN-PROGRESS" . org-todo-inprogress)
		  ("READY-TO-START" . org-todo-ready)
		  ("NEEDS-REVIEW" . org-todo-blocked)
		  ("BLOCKED" . org-todo-blocked)))

  (add-hook 'org-capture-prepare-finalize-hook 'org-id-store-link)

  (defun org-archive-done-tasks ()
	(interactive)
	(org-map-entries
	 (lambda ()
	   (org-archive-subtree)
	   (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
	 "/DONE|CANCELLED" 'file))

  ;;; Capture templates
  (setq org-default-notes-file (concat org-directory "/inbox.org")
        org-journal-file (concat org-directory "/journal.org"))
  (setq org-capture-templates
        `(("t" "Todo" entry (file ,org-default-notes-file)
           "* TODO %?\n  %i\n  %a"
           :prepend :kill-buffer)
          ("j" "Journal" entry (file+datetree ,org-journal-file)
           "* %?\nEntered on %U\n  %i\n  %a"
           :tree-week :kill-buffer)
          ("w" "Web site"
           entry (file+headline "~/org/inbox.org" "Web")
           "* %a :website:\n\n%U %?\n\n%:initial")))
  (defun find-org-tasks ()
    (interactive)
    (find-file org-default-notes-file))
  (defun find-org-journal ()
    (interactive)
    (find-file org-journal-file))

  (defun org-get-buffer-tags-completion-alist ()
    "This function's result should be passed as a collection to `completing-read'"
    (mapcar 'car (org-get-buffer-tags)))

  (defun org-rename-tag (old new)
    (interactive (list
                  (completing-read "Old Tag: " (org-get-tags) nil t)
                  (completing-read "New Tag: " (org-get-buffer-tags-completion-alist) nil 'confirm)))
    (when (member old (org-get-tags))
      (org-toggle-tag old 'off)
      (org-toggle-tag new 'on)))

  (defun org-rename-tags (old new)
    (interactive (list
                  (completing-read "Old Tag: " (org-get-buffer-tags-completion-alist) nil t)
                  (completing-read "New Tag: " (org-get-buffer-tags-completion-alist) nil 'confirm)))
    (org-map-entries
     (lambda () (org-rename-tag old new))
     (format "+%s" old)
     nil))

  (setq org-refile-targets
        '(("General.org" :maxlevel . 1)))

  (bind-keys
   ("C-c j" . outline-next-heading)
   ("C-c k" . outline-previous-heading)
   ("C-c h" . outline-up-heading)
   ("C-c l" . outline-show-subtree)
   ("C-c c" . org-capture)
   ("C-c C-l" . org-store-link)
   ("C-c SPC t" . find-org-tasks)
   ("C-c SPC j" . find-org-journal)
   :map org-mode-map
   ("C-c o d" . org-cut-element)
   ("C-c o c" . ibizaman/org-copy-element)
   ("<tab>" . org-cycle)))

(use-package org-protocol)
;(use-package org-protocol-capture-html
;  :ensure t)
(use-package org-capture-pop-frame
  :ensure t)

(use-package org-sticky-header
  :ensure t
  :hook (org-mode . org-sticky-header-mode))

(use-package org-download
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'org-download-enable))

(use-package ob-async
  :ensure t)

(use-package ob-python)

(use-package ob-shell)

(use-package ob-mongo
  :ensure t)

(use-package ob-http
  :ensure t
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((http . t))))

(use-package ob-tmux
  :ensure t
  :config
  (setq org-babel-default-header-args:tmux
        '((:results . "silent")))
  (setq org-babel-tmux-terminal "/Applications/iTerm.app/Contents/MacOS/iTerm2"
        org-babel-tmux-session-prefix "ob-"))

(use-package ox-pandoc
  :disabled
  :ensure t)

(use-package org-pandoc-import
  :disabled
  :ensure t
  :after ox-pandoc
  :config
  (org-pandoc-import-transient-mode 1))

(use-package org-noter
  :ensure t
  :after org)

(use-package pdf-tools
  :ensure t)

(use-package pdf-view
  :config
  (setq pdf-view-display-size 'fit-height))

(use-package org-visibility
  :after org
  :ensure t
  ;; :bind* (:map org-visibility-mode-map
  ;;              ("C-x C-v" . org-visibility-force-save) ; defaults to `find-alternative-file'
  ;;              ("C-x M-v" . org-visibility-remove))    ; defaults to undefined
  :hook (org-mode . org-visibility-mode)
  :custom
  ;; optionally change the location of the state file
  ;;(org-visibility-state-file `,(expand-file-name "/some/path/.org-visibility"))
  ;; list of directories and files to persist and restore visibility state of
  ;; (org-visibility-include-paths `(,(file-truename "~/.emacs.d/init-emacs.org")
  ;;                                 ,(file-truename "~/org")))
  ;; persist all org files regardless of location
  (org-visibility-include-regexps '("\\.org\\'"))
  ;; list of directories and files to not persist and restore visibility state of
  ;;(org-visibility-exclude-paths `(,(file-truename "~/org/old")))
  ;; optionally set maximum number of files to keep track of
  ;; oldest files will be removed from the state file first
  (org-visibility-maximum-tracked-files 100)
  ;; optionally set maximum number of days (since saved) to keep track of
  ;; files older than this number of days will be removed from the state file
  (org-visibility-maximum-tracked-days 180)
  ;; optionally turn off visibility state change messages
  ;;(org-visibility-display-messages nil)
  )

(use-package ob-mermaid
  :ensure t
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((mermaid . t))))

;; TODO: not found by :ensure
;; (use-package org-fc
;;   :ensure t
;;   :custom
;;   (org-fc-directories '("~/org/"))
;;   :config
;;   (require 'org-fc-hydra))

;;; Base packages

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)))

(when (daemonp)
  (use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)))

(defun my/focus-new-client-frame ()
  (select-frame-set-input-focus (selected-frame)))

(add-hook 'server-after-make-frame-hook #'my/focus-new-client-frame)

(progn
  ;; Tell Tramp to use the value of "echo $PATH", which works better for
  ;; NixOS machines using home-manager which does not set "getconf PATH"
  ;; correctly.
  (require 'tramp-sh)
  (setq tramp-remote-path
        (append tramp-remote-path
                '(tramp-own-remote-path)))
  )

(use-package delight
  :ensure t
  :config
  (delight-major-mode))

(use-package general
  :ensure t

  :config
  (general-create-definer spc-leader
   :prefix "SPC"))

(use-package multiple-cursors
  :ensure t)

(use-package undo-tree
  :ensure t
  :delight undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-enable-undo-in-region t
        undo-tree-visualizer-diff t
        undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "/undo-tree"))))

  (defun my-undo-tree-save-history (undo-tree-save-history &rest args)
    (let ((message-log-max nil)
          (inhibit-message t))
      (apply undo-tree-save-history args)))

  (advice-add 'undo-tree-save-history :around 'my-undo-tree-save-history))

(use-package autorevert
  :delight auto-revert-mode
  :config
  (global-auto-revert-mode))

(progn
 (setq backup-directory-alist
       `(("." . ,(concat user-emacs-directory "backups")))))

(use-package evil
  :ensure t
  :after (undo-tree org)
  :init
  (setq evil-want-integration t)  ; needed for evil-collection
  (setq evil-want-keybinding nil) ; needed for evil-collection
  (setq-default evil-symbol-word-search t)
  (setq evil-symbol-word-search t)
  :config
  (evil-mode 1)
  (evil-define-key 'normal Info-mode-map (kbd "]") #'Info-forward-node)
  (evil-define-key 'normal Info-mode-map (kbd "[") #'Info-backward-node)
  (evil-define-key 'normal 'global "gt" 'counsel-semantic-or-imenu)
  (evil-define-key 'normal text-mode-map (kbd "C-k") 'dwim-move-up)
  (evil-define-key 'normal text-mode-map (kbd "C-j") 'dwim-move-down)
  )

(use-package evil-collection
  :ensure t
  :after evil ;forge
  :config
  (evil-collection-init)
  (evil-collection-unimpaired-mode -1))

(eval-after-load 'treemacs
  '(progn
     (evil-define-key 'normal treemacs-mode-map (kbd "x") 'treemacs-delete-file)
     (evil-define-key 'normal treemacs-mode-map (kbd "n") 'treemacs-add-project-to-workspace)
     (evil-define-key 'normal treemacs-mode-map (kbd "cf") 'treemacs-create-file)
     (evil-define-key 'normal treemacs-mode-map (kbd "cd") 'treemacs-create-dir)
     (evil-define-key 'normal treemacs-mode-map (kbd "r") 'treemacs-rename)
     (evil-define-key 'normal treemacs-mode-map (kbd "R") 'treemacs-move-file)
     ))

(use-package flycheck
  :ensure t
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
  :ensure t
  :config
  (yas-global-mode))

(use-package flyspell
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
  :ensure t
  :delight company-mode
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t
        company-insertion-triggers nil
        company-format-margin-function 'company-vscode-dark-icons-margin
        company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                            company-echo-metadata-frontend
                            company-preview-if-just-one-frontend)
        company-search-regexp-function 'company-search-words-in-any-order-regexp)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t)

;;; https://github.com/clojure-emacs/cider/issues/2908#issuecomment-709691997
(with-eval-after-load 'company
  (add-hook 'evil-local-mode-hook
            (lambda ()
              ;; Note:
              ;; Check if `company-emulation-alist' is in
              ;; `emulation-mode-map-alists', if true, call
              ;; `company-ensure-emulation-alist' to ensure
              ;; `company-emulation-alist' is the first item of
              ;; `emulation-mode-map-alists', thus has a higher
              ;; priority than keymaps of evil-mode.
              ;; We raise the priority of company-mode keymaps
              ;; unconditionally even when completion is not
              ;; activated. This should not cause problems,
              ;; because when completion is activated, the value of
              ;; `company-emulation-alist' is ((t . company-my-keymap)),
              ;; when completion is not activated, the value is ((t . nil)).
              (when (memq 'company-emulation-alist emulation-mode-map-alists)
                (company-ensure-emulation-alist)))))

(use-package company-posframe
  :ensure t
  :delight company-posframe
  :config
  (company-posframe-mode 1))

(use-package magit
  :ensure t
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1
        magit-published-branches nil)

  ;;; Sections
  (magit-add-section-hook 'magit-status-sections-hook 'magit-insert-local-branches nil t)
  ;; (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-assigned-pullreqs nil t)
  ;; (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-assigned-issues nil t)

  ;;; Keybindings
  (evil-define-key 'normal 'global (kbd "C-x g g") 'magit-status)
  (evil-define-key 'normal 'global (kbd "C-x g d") 'magit-diff-buffer-file)
  (evil-define-key 'normal 'global (kbd "C-x g b") 'magit-blame)
  (evil-define-key 'normal 'global (kbd "C-x g l") 'magit-log)
  ;; (evil-define-key 'normal magic-mode-map (kbd "RET") 'code-review-comment-add-or-edit)
  (evil-define-key 'normal magit-mode-map (kbd "o") 'magit-visit-thing)
  (evil-define-key 'normal magit-mode-map (kbd "O") 'magit-diff-visit-file-other-window)

  (defun magit-fetch-and-reset-main ()
    "Fetch upstream and reset main branch to its upstream."
    (interactive)
    (let* ((main (magit-main-branch))
           (main-upstream (magit-get-upstream-branch main)))
    (magit-branch-reset main main-upstream)))

  (setq my/magit-clone-root-dir "~/go/src")

  (defun my/magit-clone-default-directory (remote)
    (if-let ((split (split-string remote ":"))
           (forge (car (last (split-string (nth 0 split) "@"))))
           (repo-name (file-name-sans-extension (car (last split))))
           (org (car (split-string repo-name "/")))
           (full-path (concat (file-name-as-directory my/magit-clone-root-dir)
                              (file-name-as-directory forge)
                              (file-name-as-directory org))))
      full-path
      default-directory
      ))
  (setq magit-clone-default-directory 'my/magit-clone-default-directory)

  ;;; Transient
  (transient-append-suffix 'magit-reset "w" '("M" "main branch" magit-fetch-and-reset-main))
  )

(use-package forge
  :ensure t
  :after magit
  :config
  ;; (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-assigned-pullreqs nil t)
  ;; (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-assigned-issues nil t)
  ;; (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-authored-issues nil t)
  (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-pullreqs nil t)
  ;; (remove-hook 'magit-status-sections-hook 'forge-insert-pullreqs)
  ;; (remove-hook 'magit-status-sections-hook 'forge-insert-issues)
  (setq forge-alist (append '(("ibizaman.github.com" "api.github.com" "github.com" forge-github-repository)) forge-alist))
  )

;; Currently this fails, see: https://github.com/wandersoncferreira/code-review/issues/245
;; (use-package code-review
;;  :ensure t
;;  :after magit
;;  :config
;;  (add-hook 'code-review-mode-hook #'emojify-mode)
;;  ;; (define-key code-review-mode-map (kbd "RET") nil)
;;  ;; (define-key forge-topic-mode-map (kbd "RET") 'code-review-forge-pr-at-point)
;;  (evil-define-key '(normal visual) code-review-mode-map (kbd "r") 'code-review-transient-api)
;;  ;; (define-key code-review-mode-map (kbd "RET") 'code-review-comment-add-or-edit)
;;  (evil-define-key 'normal code-review-mode-map (kbd "RET") 'code-review-comment-add-or-edit)
;;  (evil-define-key 'normal code-review-mode-map (kbd "C-n") 'code-review-comment-jump-next)
;;  (evil-define-key 'normal code-review-mode-map (kbd "C-p") 'code-review-comment-jump-previous)
;;  )

(use-package auth-source)

(use-package auth-source-pass
  :config
  (auth-source-pass-enable))

(use-package ivy
  :ensure t
  :delight ivy-mode
  :config
  (define-key ivy-mode-map (kbd "C-s") 'swiper-thing-at-point)
  (define-key ivy-mode-map (kbd "C-<return>") 'ivy-immediate-done)
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order)))
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package wgrep
  :ensure t)

(use-package counsel
  :ensure t
  :delight counsel-mode
  :after ivy
  :config
  (counsel-mode 1))

(use-package deadgrep
  :ensure t
  :config
  (global-set-key (kbd "C-c g") #'deadgrep))

(use-package projectile
  :ensure t
  :delight '(:eval (format " [%s]" (projectile-project-name)))
  :config
  (projectile-mode 1)
  (setq projectile-file-exists-local-cache-expire (* 5 60)))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; (use-package persp-projectile
;;   :ensure t
;;   :config
;;   (persp-mode))

(use-package which-key
  :ensure t
  :delight which-key-mode
  :config
  (which-key-mode 1))

(use-package eldoc
  :delight eldoc-mode)

(use-package eyebrowse
  :ensure t
  :config
  (setq eyebrowse-new-workspace t)
  (eyebrowse-mode 1))

(use-package activities
  :ensure t
  :init
  (activities-mode)
  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)

  :bind
  (("C-x C-a C-n" . activities-new)
   ("C-x C-a C-x" . activities-resume)
   ("C-x C-a C-s" . activities-suspend)
   ("C-x C-a C-k" . activities-kill)
   ;; This binding mirrors, e.g. "C-x t RET".
   ("C-x C-a RET" . activities-switch)
   ("C-x C-a g" . activities-revert)
   ("C-x C-a l" . activities-list)))

(use-package dired
  :config
  (setq dired-dwim-target 'dired-dwim-target-recent))

;; (use-package dired-plus
;;   :ensure t)

(use-package xref
  :config

  (defcustom xref-pop-bury-buffer nil
	"Toggles if popping a buffer buries it also."
	:type 'boolean)

  (defun xref-pop-marker-stack ()
	"Pop back to where \\[xref-find-definitions] was last invoked."
	(interactive)
	(let ((ring xref--marker-ring))
	  (when (ring-empty-p ring)
		(user-error "Marker stack is empty"))
	  (let ((marker (ring-remove ring 0)))
		(when xref-pop-bury-buffer
		  (bury-buffer))
		(switch-to-buffer (or (marker-buffer marker)
							  (user-error "The marked buffer has been deleted")))
		(goto-char (marker-position marker))
		(set-marker marker nil nil)
		(run-hooks 'xref-after-return-hook))))

  (setq xref-pop-bury-buffer t))

(use-package popper
  :ensure t
  :after evil
  :init
  (defun popper-display-popup-on-right (buffer &optional alist)
    "Display popup-buffer BUFFER on the right of the screen."
    (display-buffer-in-side-window
     buffer
     (append alist
             `((window-height . ,popper-window-height)
               (side . right)
               (slot . 1)))))

  (setq popper-reference-buffers '("\\*Messages\\*"
								   "Output\\*$"
								   help-mode
								   compilation-mode
								   go-test-mode)
		popper-group-function #'popper-group-by-projectile
        popper-display-function #'popper-display-popup-on-right)

  :config
  (popper-mode 1)

;;  (spc-leader
;;	:keymaps 'normal
;;	"p" #'popper-cycle
;;	"SPC" #'popper-toggle-latest)
)

(use-package shackle
  :ensure t
  :config
  (shackle-mode 1)
  ;; (setq shackle-default-alignment 'right
  ;; 		shackle-rules '())
  )

(defun file-notify-rm-all-watches ()
  "Remove all existing file notification watches from Emacs."
  (interactive)
  (maphash
   (lambda (key _value)
     (file-notify-rm-watch key))
   file-notify-descriptors))

;; (use-package impatient-mode
;;   :ensure t)

;;; UI

(blink-cursor-mode 0)
(setq use-dialog-box nil)

;; Don't use messages that you don't read
(setq initial-scratch-message ""
      inhibit-startup-message t)

(setq visible-bell t
      ring-bell-function 'ignore)

(setq inhibit-startup-echo-area-message "ibizaman")

; (toggle-frame-fullscreen)

(menu-bar-mode 0)
(tool-bar-mode 0)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))

;; Set fonts even in daemon mode thanks to the hooks. Comes from
;; https://emacs.stackexchange.com/a/64499/16879.
;; after-make-frame-functions hook works in daemon mode and
;; window-setup-hook works in non-daemon mode.
(progn
  (defun configure-fonts (&optional frame)
    (when (memq window-system '(mac ns x))
      (cond
       ((x-list-fonts "*-Inconsolata-*") (set-face-attribute 'default nil :font "Inconsolata-11"))
       ((x-list-fonts "*-Inconsolata Nerd Font-*") (set-face-attribute 'default nil :font "Inconsolata Nerd Font-9"))
       ((x-list-fonts "*-InconsolataG-*") (set-face-attribute 'default nil :font "InconsolataG-10")))))

  (add-hook 'after-make-frame-functions #'configure-fonts)
  (add-hook 'window-setup-hook #'configure-fonts))


(setq kill-do-not-save-duplicates t
      ; From https://stackoverflow.com/a/29092845/1013628
      select-enable-clipboard t)

(defun my/disable-tabs ()
  "Disable tabs and set them to 4 spaces."
  (setq-default tab-width 4)
  (setq tab-width 4)
  (setq-default indent-tabs-mode nil)
  (setq indent-tabs-mode nil))
;; Tabs are used to format buffer with `lsp-format-buffer'.
(my/disable-tabs)

(use-package rainbow-mode
  :ensure t
  :config
  (rainbow-mode 1))

(use-package rainbow-delimiters
  :ensure t
  :config
  (rainbow-delimiters-mode 1))

(setq-default fill-column 100)

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
  ; (hidden-mode-line-mode 1)
  ; (add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)

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

;; (straight-use-package
;;   '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))
;; ; (require 'nano)

(use-package mustang-theme
  :ensure t
  :config
  (load-theme 'mustang t))

;;; Miscellaneous

;; Shortcuts to files
(defun find-file-marker (file)
  "Find FILE but mark the current point first."
  `(lambda ()
	 (interactive)
	 (xref-push-marker-stack)
	 (find-file ,file)))

;; Use only one space after a dot
(setq sentence-end-double-space nil)

;; Delete trailing whitespace
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

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

;; Maximize startup window
;; https://emacs.stackexchange.com/a/3017/16879
(add-to-list 'initial-frame-alist '(fullscreen . maximized))


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

(defun compile-at (arg)
  ""
  (interactive "P")
  (let* ((default-directory (read-directory-name "Compile directory: " (magit-toplevel)))
		 (default-directory (if (file-exists-p default-directory)
								(file-name-directory default-directory)
							  default-directory)))
	(call-interactively #'compile)))


;;; Helper packages

(use-package expand-region
  :ensure t
  :config
  (defun me/expand-region ()
    (interactive)
    (er--expand-region-1))
  (defun me/contract-region ()
    (interactive)
    (er/contract-region 1))
  (keymap-global-set "C-c C-=" #'me/expand-region)
  (keymap-global-set "C-c C--" #'me/contract-region))

(defun use-region-or-expand-region ()
  "Use region if active or expand region at point."
  (when (not (use-region-p))
    (let ((inhibit-message t))
      (call-interactively 'er/expand-region))))

(use-package helpful
  :ensure t
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
  :bind (("C-x C-b" . 'ibuffer)))


(use-package git-link
  :ensure t
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


;; TODO: use ensure
;; (use-package emacs-org-babel-conf
;;   :straight (emacs-org-babel-conf :type git :host github :repo "ibizaman/emacs-org-babel-conf" :branch "master"))


(use-package hydra
  :ensure t)


(use-package hledger-mode
  :ensure t
  :config
  (setq hledger-jfile "~/Projects/hledger/hledger.journal")
  (setenv "LEDGER_FILE" "~/Projects/hledger/hledger.journal")
  (add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))
  (add-to-list 'company-backends 'hledger-company)

  ; B gets in the way too much
  ; (evil-define-key 'normal hledger-mode-map (kbd "B") 'hledger-balancesheet*)
  (evil-define-key 'normal hledger-mode-map (kbd "D") 'hledger-reschedule)
  (evil-define-key 'normal hledger-view-mode-map (kbd "d") 'hledger-report-at-day)
  (evil-define-key 'normal hledger-view-mode-map (kbd "n") 'hledger-next-report)
  (evil-define-key 'normal hledger-view-mode-map (kbd "p") 'hledger-prev-report))


(use-package csv-mode
  :ensure t)

(use-package emacs-conflict
  :ensure t)

(use-package copy-as-format
  :ensure t)

(use-package gif-screencast
  :ensure t
  :config
  :bind (("<f7>" . gif-screencast-start-or-stop)
         ("<f8>" . gif-screencast-toggle-pause)
         ("<f9>" . gif-screencast-stop))
  :config
  (when (memq window-system '(ns))
	  (setq gif-screencast-args '("-x")) ;; To shut up the shutter sound of `screencapture' (see `gif-screencast-command').
	  (setq gif-screencast-cropping-program "mogrify") ;; Optional: Used to crop the capture to the Emacs frame.
	  (setq gif-screencast-capture-format "ppm")) ;; Optional: Required to crop captured images.
  )

(use-package edit-server
  :ensure t)

;;; Mu4e

(if (memq window-system '(mac ns))
    (add-to-list 'load-path "/usr/local/Cellar/mu/1.0/share/emacs/site-lisp/mu/mu4e")
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e"))

(if-let* ((muexec (executable-find "mu"))
		  (mu4epath
		   (concat
			(f-dirname
			 (file-truename
			  muexec))
			"/../share/emacs/site-lisp/mu4e")))
	(when (and
		   (string-prefix-p "/nix/store/" mu4epath)
		   (file-directory-p mu4epath))
	  (add-to-list 'load-path mu4epath)))

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
                                         ( (:name  "Unread messages not list"
                                            :query (lambda () (ibizaman/mu4e-generate-unread-filter))
                                            :key ?u)
                                           (:name  "Recruiting"
                                            :query "maildir:/Gmail/recruiting"
                                            :key ?r)
                                           (:name  "Unread messages all"
                                            :query (concat "flag:unread "
                                                           "AND NOT flag:trashed"
                                                           "AND NOT maildir:\"/Gmail/[Google Mail].Trash\" "
                                                           "AND NOT maildir:\"/Gmail/[Google Mail].Spam\" ")
                                            :key ?i)
                                           (:name  "Unread list messages"
                                            :query (lambda () (call-interactively 'ibizaman/mu4e-get-unread-list-filter-query))
                                            :key ?l)
                                           (:name "Today's messages"
                                            :query "date:today..now"
                                            :key ?t)
                                           (:name "Last 7 days"
                                            :query "date:7d..now AND NOT flag:list AND NOT maildir:/Gmail/recruiting"
                                            :key ?w)
                                           (:name "Messages with images"
                                            :query "mime:image/*"
                                            :key ?p)
                                           (:name "Drafts"
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
        smtpmail-smtp-user "ibizapeanut@gmail.com")

  (global-set-key (kbd "C-x m") #'mu4e))

;; (use-package org-mu4e
;;   :after (org mu4e))

;;; Language specific packages

(global-set-key (kbd "C-c C-j") 'find-function)

;;;; Language Server Protocol

(defun my/lsp-format-buffer-silent ()
  "Silence errors from `lsp-format-buffer'."
  (ignore-errors (lsp-format-buffer)))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-x l")
  :hook (lsp-enable-which-key-integration
		 (sh-mode . lsp-deferred)
         (javascript-mode . lsp-deferred)
         (html-mode . lsp-deferred)
		 (sh-mode . lsp-deferred)
         ;; (before-save . my/lsp-format-buffer-silent)
         (lsp-mode . lsp-enable-which-key-integration)
		 (nix-mode . lsp-deferred)
		 (kotlin-mode . lsp-deferred)
		 (python-mode . lsp-deferred))
  :config
  (setq lsp-signature-auto-activate t
		lsp-ui-doc-show-with-cursor t)
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq gc-cons-threshold 100000000
        read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-warn-no-matched-clients nil
        lsp-disabled-clients (add-to-list 'lsp-disabled-clients 'golangci-lint))
  (lsp-treemacs-sync-mode 1)
  (lsp-lens-mode nil))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode-hook . lsp-ui-mode)
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-flycheck-enable t
        lsp-ui-flycheck-live-reporting nil))

;; (use-package company-lsp
;;   :ensure t
;;   :commands company-lsp
;;   :config
;;   (push 'company-lsp company-backends))

;;;; Elisp

(defun eval-point-region-and-deactivate ()
  "Evaluate region or expanded region and deactivates region when done."
  (interactive)
  (use-region-or-expand-region)
  (condition-case-unless-debug err
      (message "%s" (call-interactively 'eval-region))
    (error (deactivate-mark)
           (signal (car err) (cdr err))))
  (deactivate-mark))


(use-package elisp-mode
  :config
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-point-region-and-deactivate))

(use-package dap-mode
  :ensure t)

;;;; Haskell

(use-package nix-sandbox
  :ensure t)

(use-package haskell-mode
  :ensure t
  :after nix-sandbox
  :init
  (defun my/haskell-set-stylish ()
    (if-let* ((sandbox (nix-current-sandbox))
              (fullcmd (nix-shell-command sandbox "stylish-haskell"))
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
        haskell-stylish-on-save t
		lsp-haskell-formatting-provider "stylish-haskell"))

(use-package lsp-haskell
  :ensure t
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
  :ensure t
  :mode "\\.nix\\'"
  :init
  (require 'nix-build)
  :config
  (defun nix-search-package (arg)
    (interactive "P")
    (let* ((package (thing-at-point 'filename t))
           (url (format "https://search.nixos.org/packages?channel=22.11&from=0&size=50&sort=relevance&type=packages&query=%s" package)))
      (if url
          (browse-url url (if arg
                              (not browse-url-new-window-flag)
                            browse-url-new-window-flag))
        (error "No URL found")))))

;;;; Sops

(defvar random-secret-openssl-executable "openssl"
  "OpenSSL executable for generating random secret. Example: 'nix run nixpkgs#openssl --'")

(setq random-secret-openssl-executable "nix run nixpkgs#openssl -- ")

(defun insert-random-secret (&optional length)
  "Insert at point a random secret of given LENGTH."
  (interactive)
  (unless length (setq length 64))
  (insert (shell-command-to-string (format "%s rand -hex %d" random-secret-openssl-executable length))))

;;;; ELM

(use-package elm-mode
  :ensure t
  :hook
  ;; (elm-mode . my/elm-mode-local-config)
  :config
  (defun my/nix--elm-wrapper (args)
    (if-let ((sandbox (nix-current-sandbox)))
        (apply 'nix-shell-string sandbox args)
      args))

  (defun my/nix--elm-wrapper-list (args)
    (if-let ((sandbox (nix-current-sandbox)))
        (apply 'nix-shell-command sandbox args)
      args))

  (defun elm-compile--command (file &optional output json)
    "Generate a command that will compile FILE into OUTPUT.
When JSON is non-nil, JSON reporting will be enabled."
    (let ((elm-compile-arguments
           (if output
               (append (cl-remove-if (apply-partially #'string-prefix-p "--output=") elm-compile-arguments)
                       (list (concat "--output=" (expand-file-name output))))
             elm-compile-arguments)))
      (my/nix--elm-wrapper (append (elm--ensure-list elm-compile-command)
                                   (append (list file)
                                           elm-compile-arguments
                                           (when json
                                             (list "--report=json")))))))

  (defun elm-reactor ()
    "Run the Elm reactor process."
    (interactive)
    (let ((default-directory (elm--find-dependency-file-path))
          (cmd (my/nix--elm-wrapper-list (elm--expand-args (append (elm--ensure-list elm-reactor-command) elm-reactor-arguments)))))
      (with-current-buffer (get-buffer-create elm-reactor--buffer-name)
        (comint-mode)
        (ansi-color-for-comint-mode-on)
        (let ((proc (get-buffer-process (current-buffer))))
          (if (and proc (process-live-p proc))
              (progn
                (message "Restarting elm-reactor")
                (delete-process proc))
            (message "Starting elm-reactor")))

        (let ((proc (apply #'start-process "elm reactor" elm-reactor--buffer-name
                           (car cmd) (cdr cmd))))
          (when proc
            (set-process-filter proc 'comint-output-filter))))))
  )




;;;; Markdown

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gf-mode)
  :hook (markdown-mode . lsp-deferred)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;;; Mermaid

(use-package mermaid-mode
  :ensure t)

;;;; Yaml


(use-package outline-magic
  :ensure t)

(use-package yaml-mode
  :ensure t
  :hook (yaml-mode . lsp-deferred)
  :config

  ;; From https://gist.github.com/leoc/f8c0868051003c4ea6eff638bc614575
  (add-hook 'yaml-mode-hook 'leoc/yaml-outline-hook)

  ;; Customize folding markers
  (set-display-table-slot
   standard-display-table
   'selective-display
   (let ((face-offset (* (face-id 'shadow) (lsh 1 22))))
     (vconcat (mapcar (lambda (c) (+ face-offset c)) " [+]"))))

  (defun leoc/yaml-outline-level ()
    (s-count-matches "\\([ ]\\{2\\}\\)" (match-string 0)))

  (defun leoc/yaml-outline-hook ()
    (interactive)
    (setq outline-regexp
          (rx
           (seq
            bol
            (group (zero-or-more "  ")
                   (or (group
                        (seq (or (seq "\"" (*? (not (in "\"" "\n"))) "\"")
                                 (seq "'" (*? (not (in "'" "\n"))) "'")
                                 (*? (not (in ":" "\n"))))
                             ":"
                             (?? (seq
                                  (*? " ")
                                  (or (seq "&" (one-or-more nonl))
                                      (seq ">-")
                                      (seq "|"))
                                  eol))))
                       (group (seq
                               "- "
                               (+ (not (in ":" "\n")))
                               ":"
                               (+ nonl)
                               eol)))))))

    (setq outline-level 'leoc/yaml-outline-level)

    (outline-minor-mode t)
    (hide-body)
    (show-paren-mode 1)
    (define-key yaml-mode-map [tab] 'outline-cycle)
    (define-key outline-minor-mode-map [M-S-tab] 'indent-for-tab-command)
    (define-key outline-minor-mode-map [M-down] 'outline-move-subtree-down)
    (define-key outline-minor-mode-map [M-up] 'outline-move-subtree-up)))

;;;; Jenkins

(use-package jenkinsfile-mode
  :ensure t)

;;;; Go

(use-package go-mode
  :ensure t
  :after nix-sandbox
  :hook (go-mode . lsp-deferred)
  :bind (([remap find-function] . 'lsp-find-definition))
  :config
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

  (defun my/lsp-go-exclude-vendor-directory ()
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]vendor\\'")
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]dashboard/ui\\'")
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]dashboard/assets\\'")
    (setq lsp-file-watch-threshold 50000))
  (add-hook 'go-mode-hook #'my/lsp-go-exclude-vendor-directory)
  (add-hook 'markdown-mode-hook #'my/lsp-go-exclude-vendor-directory)

  ; https://emacs-lsp.github.io/lsp-mode/page/lsp-gopls/#configuration
  (setq lsp-go-analyses '((shadow . t)
                          (nilness . t)
                          (unusedparams . t)
                          (unusedwrite . t)
                          (unusedvariable . t)))

  ;; (setq flycheck-go-golint-executable "golangci-lint run")
  ;; (setq lsp-go-directory-filters '("-vendor"))
  ;; (setq lsp-go-directory-filters "[- +dataservices]"))
  ;; (setq lsp-go-directory-filters "[]")
  ;; (lsp-register-custom-settings
  ;;  '(("gopls.directoryFilters" lsp-go-directory-filters)))

  (defun my/nix--lsp-go-wrapper (args)
    (if-let ((sandbox (nix-current-sandbox)))
        (apply 'nix-shell-command sandbox args)
      args))
  (setq lsp-go-gopls-server-path "gopls"
        lsp-go-server-wrapper-function 'my/nix--lsp-go-wrapper)
  )

; Not sure I want this, takes a lot of RAM for not much.
; (with-eval-after-load 'lsp-mode
;   ;; requires: go install -mod=readonly github.com/nametake/golangci-lint-langserver
;   ;; requires: go install -mod=readonly golang.org/x/lint/golint@latest
;   (lsp-register-custom-settings
;    '(("golangci-lint.command"
;       ["golangci-lint" "run" "--out-format" "json" "--allow-parallel-runners"])))
; 
;   (lsp-register-client
;    (make-lsp-client :new-connection (lsp-stdio-connection
;                                      '("golangci-lint-langserver"))
;                     :major-modes '(go-mode)
;                     :language-id "go"
;                     :priority 0
;                     :server-id 'golangci-lint
;                     :add-on? t
;                     :library-folders-fn #'lsp-go--library-default-directories
;                     :initialization-options (lambda ()
;                                               (gethash "golangci-lint"
;                                                        (lsp-configuration-section "golangci-lint")))))
; 
;   (add-to-list 'lsp-language-id-configuration '(go-mode . "golangci-lint")))


(defun nix-go-test-go-command ()
  "Set go-test go executable using nix."
  (setq-local go-test-go-command (nix-executable-find (nix-current-sandbox) "go")))

(use-package gotest
  :ensure t
  :after (company go-mode)
  :hook ((go-mode . nix-go-test-go-command))
  :config
  (defun go-test-subtest-name ()
	"Returns the full name of the subtest under point if any, or the test name."
	(cl-destructuring-bind (test-suite test-name) (go-test--get-current-test-info)
	  (let ((subtest (go-test--read-subtest-name)))
		(if (string-blank-p subtest)
			test-name
		  (message "%s/%s" test-name subtest)))))

  (defun go-test--read-subtest-name()
	"Read the string under point and replaces whitespaces with underscores."
	(if (not (er--point-inside-string-p))
		""
	  (er/mark-inside-quotes)
	  (let* ((rawname (buffer-substring-no-properties (region-beginning) (region-end)))
			 (name (replace-regexp-in-string " " "_" rawname)))
		(deactivate-mark)
		name)))

  )

(use-package go-dlv
  :ensure t
  :after gotest)

(use-package ob-go
  :ensure t)

(eval-after-load 'hydra
  (eval-after-load 'gotest
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

        (defun go-test-args-tags-arg (&optional testprefix)
          "Get tags argument."
          (if (null go-test-args-tags) ""
            (format "-%stags=\"%s\""
					(if testprefix "test." "-")
					(mapconcat 'identity (symbol-value 'go-test-args-tags) " "))))


        (defcustom go-test-args-base-cpu '("1" "2" "3" "4" "5" "6" "7" "8")
          "Available tags that can be toggled."
          :group 'go-test-args
          :type '(repeat string))

        (defvar go-test-args-cpu nil
          "Go test arg --cpu=")

        (defun go-test-args-cpu-toggle ()
          "Toggle cpu argument."
          (go-test-args--toggle-list 'go-test-args-cpu 'go-test-args-base-cpu))

        (defun go-test-args-cpu-arg (&optional testprefix)
          "Get cpu argument."
          (if (null go-test-args-cpu) ""
            (format "-%scpu=%s"
					(if testprefix "test." "-")
					(mapconcat 'identity (symbol-value 'go-test-args-cpu) ","))))


        (defvar go-test-args-verbose nil
          "Go test arg -v")

        (defun go-test-args-verbose-toggle ()
          "Toggle verbose argument."
          (go-test-args--toggle-bool 'go-test-args-verbose))

        (defun go-test-args-verbose-arg (&optional testprefix)
          "Get verbose argument."
          (if go-test-args-verbose (format "-%sv" (if testprefix "test." "")) ""))


        (defvar go-test-args-race nil
          "Go test arg --race")

        (defun go-test-args-race-toggle ()
          "Toggle race argument."
          (go-test-args--toggle-bool 'go-test-args-race))

        (defun go-test-args-race-arg (&optional testprefix)
          "Get race argument."
          (if go-test-args-race
			  (format"-%srace" (if testprefix "test." "-")) ""))


        (defvar go-test-args-count nil
          "Go test arg --count")

        (defun go-test-args-count-toggle ()
          "Toggle count argument."
          (go-test-args--toggle-number 'go-test-args-count))

        (defun go-test-args-count-arg (&optional testprefix)
          "Get count argument."
          (if (null go-test-args-count) ""
            (format "-%scount=\"%s\""
					(if testprefix "test." "-")
					go-test-args-count)))


        (defvar go-test-args-parallel nil
          "Go test arg --parallel")

        (defun go-test-args-parallel-toggle ()
          "Toggle parallel argument."
          (go-test-args--toggle-number 'go-test-args-parallel))

        (defun go-test-args-parallel-arg (&optional testprefix)
          "Get parallel argument."
          (if (null go-test-args-parallel) ""
            (format "-%sparallel=\"%s\""
					(if testprefix "test." "-")
					go-test-args-parallel)))


        (defvar go-test-args-coverprofile nil
          "Go test arg --coverprofile")

        (defun go-test-args-coverprofile-toggle ()
          "Toggle coverprofile argument."
          (go-test-args--toggle-str 'go-test-args-coverprofile))

        (defun go-test-args-coverprofile-arg (&optional testprefix)
          "Get coverprofile argument."
          (if (null go-test-args-coverprofile) ""
            (format "-%scoverprofile=\"%s\""
					(if testprefix "test." "-")
					go-test-args-coverprofile)))


        (defvar go-test-args-coverpkg nil
          "Go test arg --coverpkg=")

        (defun go-test-args-coverpkg-toggle ()
          "Toggle coverpkg argument."
          (go-test-args--toggle-list 'go-test-args-coverpkg ()))

        (defun go-test-args-coverpkg-arg (&optional testprefix)
          "Get coverpkg argument."
          (if (null go-test-args-coverpkg) ""
			(format "-%scoverpkg=\"%s\""
					(if testprefix "test." "-")
					(mapconcat 'identity (symbol-value 'go-test-args-coverpkg) ","))))


        (defvar go-test-args-custom nil
          "Custom test arguments")

        (defun go-test-args-custom-toggle ()
          "Toggle custom test argument."
          (go-test-args--toggle-str 'go-test-args-custom))

        (defun go-test-args-custom-arg ()
          "Get custom test argument."
          (if (null go-test-args-custom) ""
            (symbol-value 'go-test-args-custom)))


        (defvar go-test-args-run nil
          "Test name to run")

        (defun go-test-args-run-toggle ()
          "Toggle run argument."
			(go-test-args--toggle-str 'go-test-args-run))

		(defun go-test-run-one ()
		  "Run TESTNAME."
		  (let ((go-test--current-test-cache `("" ,go-test-args-run)))
			(go-test-current-test 'last)))


		(defun go-test-debug-one ()
		  "Run TESTNAME."
          (when (string-empty-p go-test-args-run)
            (error "Please set the -run argument first"))
		  (let ((cmd (concat "dlv test -- -test.run=" go-test-args-run " " (go-test-args--all-args t))))
			(message cmd)
			(dlv cmd)))


		(defun go-test-args--all-args (&optional prefix)
          "Set go-test-args from variables toggled in this module."
          (let* ((allvalues `(,(go-test-args-tags-arg prefix)
                              ,(go-test-args-count-arg prefix)
                              ,(go-test-args-parallel-arg prefix)
                              ,(go-test-args-verbose-arg prefix)
                              ,(go-test-args-race-arg prefix)
                              ,(go-test-args-cpu-arg prefix)
                              ,(go-test-args-coverprofile-arg prefix)
                              ,(go-test-args-coverpkg-arg prefix)
                              ,(go-test-args-custom-arg)))
                 (nonnil (seq-filter (lambda (elem) (not (or (null elem) (equal elem ""))))
                                     allvalues)))
            (mapconcat 'identity nonnil " ")))

        (defun go-test-args--set-global-var (&optional prefix)
          "Set go-test-args from variables toggled in this module."
            (setq go-test-args (go-test-args--all-args)))


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
          (let ((value (read-string "Enter a value (leave empty to unset the argument): " (go-test-subtest-name))))
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
r_a_ce:            %`go-test-args-race
cp_u_:             %`go-test-args-cpu
c_o_verprofile:    %`go-test-args-coverprofile
coverp_k_g:        %`go-test-args-coverpkg
_c_ustom:          %`go-test-args-custom
_R_un:             %`go-test-args-run

go-test-args: %s(symbol-value 'go-test-args)

_t_: run current _t_est   _b_: run current _b_enchmark   _O_: show coverage
_f_:      ... in _f_ile   _B_:           ^^... in file   _r_: run RUN
_p_:   ... in _p_ackage   _N_:        ^^... in package   _d_: debug RUN
"
          ("T" (go-test-args-tags-toggle))
          ("C" (go-test-args-count-toggle))
          ("P" (go-test-args-parallel-toggle))
          ("v" (go-test-args-verbose-toggle))
          ("a" (go-test-args-race-toggle))
          ("u" (go-test-args-cpu-toggle))
          ("o" (go-test-args-coverprofile-toggle))
          ("k" (go-test-args-coverpkg-toggle))
          ("c" (go-test-args-custom-toggle))
          ("R" (go-test-args-run-toggle))

          ("t" (go-test-current-test) :color blue)
          ("f" (go-test-current-file) :color blue)
          ("p" (go-test-current-project) :color blue)
          ("b" (go-test-current-benchmark) :color blue)
          ("B" (go-test-current-file-benchmarks) :color blue)
          ("N" (go-test-current-project-benchmarks) :color blue)
          ("O" (go-coverage) :color blue)
          ("r" (go-test-run-one) :color blue)
          ("d" (go-test-debug-one) :color blue))

        (define-key go-mode-map (kbd "C-c t") 'go-test-args-hydra/body)
        (define-key go-test-mode-map (kbd "C-c t") 'go-test-args-hydra/body))))

(defcustom go-test-test-function-regexp
  "func +Test.*(t \\*testing.T) {"
  "Regex to identify test functions."
  :type 'string
  :group 'gotest)

(defun go-test-list ()
  (interactive)
  (project-find-regexp go-test-test-function-regexp))

;;;; Java

(use-package lsp-java
  :ensure t)

(use-package kotlin-mode
  :ensure t)

;;;; PHP

(use-package php-mode
  :ensure t)

;;;; Arduino

(use-package arduino-mode
  :ensure t)

;;;; Protobuf

(use-package protobuf-mode
  :ensure t)

;;;; Flatbuffers

(use-package flatbuffers-mode
  :ensure t)

;;;; Hakyll

(defgroup hakyll-blog nil
  "Hakyll Blog."
  :group 'applications)

(defcustom hakyll-blog-dir
  "~/blog"
  "Hakyll blog directory."
  :type 'string
  :group 'hakyll-blog)

(defcustom hakyll-blog-file-time-format
  "%Y-%m-%d"
  "Hakyll blog post filename time format."
  :type 'string
  :group 'hakyll-blog)

(setq hakyll-blog-dir "~/Projects/blog")

(defun hakyll-blog-new-post (title)
  "Create new blog post under `hakyll-blog-dir' with given TITLE."
  (interactive "sBlog post title: ")
  (find-file (hakyll-blog--file-format title))
  (make-directory (hakyll-blog--post-images-dir title))
  (insert (format "---\ntitle: %s\ntags: \nwip: true\n---\n\n![image example](/images/%s)\n\n" title (hakyll-blog--sluggify-post-title title))))

(defun hakyll-blog-update-date (&optional date)
  "Update blog's date to DATE or today's date."
  (interactive)
  (let* ((current-file (buffer-file-name (current-buffer)))
         (current-title (substring (file-name-base current-file) 11)) ;; 11 for `YYYY-MM-DD-'
         (new-file (hakyll-blog--file-format current-title)))
    (rename-file current-file new-file)
    (set-visited-file-name new-file)))

(defun hakyll-blog--sluggify-post-title (title &optional time)
  "Slug of post TITLE with optional TIME or today's date."
  (format "%s-%s"
          (format-time-string hakyll-blog-file-time-format time)
          (s-dashed-words title)))

(defun hakyll-blog--file-format (title)
  "File name for TITLE post."
  (format "%s/posts/%s.markdown"
          (expand-file-name hakyll-blog-dir)
          (hakyll-blog--sluggify-post-title title)))

(defun hakyll-blog--post-images-dir (title)
  "Directory name for images of TITLE post."
  (format "%s/images/%s"
          (expand-file-name hakyll-blog-dir)
          (hakyll-blog--sluggify-post-title title)))

(defun hakyll-blog-dired-wip ()
  "List all WIP blog posts."
  (interactive)
  (find-grep-dired (concat (file-name-as-directory hakyll-blog-dir) "posts") "wip: *true"))

;;; Elfeed

(use-package elfeed
  :ensure t)

(use-package elfeed-protocol
  :ensure t

  :config
  (setq elfeed-use-curl t
        elfeed-protocol-ttrss-maxsize 200
        elfeed-feeds  `(("ttrss+https://admin@tt-rss.tiserbox.com"
                         :use-authinfo t)))
  (elfeed-set-timeout 36000)
  (elfeed-protocol-enable))

;;; Jira

(use-package org-jira
  :if (seq-some (lambda (name) (string= system-name name)) '("Pierres-MBP.tiserbox.com" "C02Z93RNLVDL"))
  :ensure t
  :config
  (make-directory "~/Documents/Jira/Fastly" t)
  (setq jiralib-url "https://fastly.atlassian.net"
        jiralib-user "pierre@fastly.com"
		org-jira-working-dir "~/Documents/Jira/Fastly"
        org-jira-use-status-as-todo t)

  (defun org-jira-get-issues-from-one-custom-jql (filename)
    ""
    (interactive (list (completing-read "Custom sql name: " (mapcar (lambda (x) (plist-get x :filename)) org-jira-custom-jqls))))
    (let ((custom-jql (cl-find filename org-jira-custom-jqls :test 'equal :key (lambda (x) (plist-get x :filename)))))
      (org-jira-get-issues-from-custom-jql (list custom-jql)))))

(setq jiralib-update-issue-fields-exclude-list '(reporter))

; (setq request-log-level 'blather)
; (setq request-message-level 'blather)

;; TODO: not found by :ensure, also wasn't updated in a long time.
;; (use-package ejira
;;   :if (seq-some (lambda (name) (string= system-name name)) '("Pierres-MBP.tiserbox.com" "C02Z93RNLVDL"))
;;   :ensure t
;;   :init
;;   (setq jiralib2-url              "https://fastly.atlassian.net"
;;         jiralib2-auth             'basic
;;         jiralib2-user-login-name  "pierre@fastly.com"
;;         jiralib2-token            nil

;;         ;; NOTE, this directory needs to be in `org-agenda-files'`
;;         ejira-org-directory       "~/Documents/ejira"
;;         ejira-projects            '("SDS")

;;         ejira-priorities-alist    '(("Highest" . ?A)
;;                                     ("High"    . ?B)
;;                                     ("Medium"  . ?C)
;;                                     ("Low"     . ?D)
;;                                     ("Lowest"  . ?E))
;;         ejira-todo-states-alist   '(("To Do"       . 1)
;;                                     ("In Progress" . 2)
;;                                     ("Done"        . 3)))
;; :config
;; ;; Tries to auto-set custom fields by looking into /editmeta
;; ;; of an issue and an epic.
;; (add-hook 'jiralib2-post-login-hook #'ejira-guess-epic-sprint-fields)

;; ;; They can also be set manually if autoconfigure is not used.
;; ;; (setq ejira-sprint-field       'customfield_10001
;; ;;       ejira-epic-field         'customfield_10002
;; ;;       ejira-epic-summary-field 'customfield_10004)

;; (require 'ejira-agenda)

;; ;; Make the issues visisble in your agenda by adding `ejira-org-directory'
;; ;; into your `org-agenda-files'.
;; (add-to-list 'org-agenda-files ejira-org-directory)

;; ;; Add an agenda view to browse the issues that
;; (org-add-agenda-custom-command
;;  '("j" "My JIRA issues"
;;    ((ejira-jql "resolution = unresolved and assignee = currentUser()"
;;                ((org-agenda-overriding-header "Assigned to me")))))))

;;; Save session

; (desktop-load-default)
; (desktop-read)

;;; Terraform

(use-package terraform-mode
  :ensure t
  :hook ((terraform-mode . lsp-deferred))
  :config
  (setq lsp-terraform-server '("terraform-ls" "serve")))

;;; Json

(use-package json-mode
  :ensure t
  :config
  (setq json-mode-wi 2)

  (defun org-babel-execute:json (body params)
    (let ((jq (cdr (assoc :jq params)))
          (node (cdr (assoc :node params))))
      (cond
       (jq
        (with-temp-buffer
          ;; Insert the JSON into the temp buffer
          (insert body)
          ;; Run jq command on the whole buffer, and replace the buffer
          ;; contents with the result returned from jq
          (shell-command-on-region (point-min) (point-max) (format "jq -r \"%s\"" jq) nil 't)
          ;; Return the contents of the temp buffer as the result
          (buffer-string)))
       (node
        (with-temp-buffer
          (insert (format "const it = %s;" body))
          (insert node)
          (shell-command-on-region (point-min) (point-max) "node -p" nil 't)
          (buffer-string)))))))

;;; Javascript

(setq js-indent-level 2)

;;; Music

(use-package lilypond-mode
  :requires lilypond-mode)

;;; SQL

;; (use-package sql-mode
;;   :ensure t)

;;; Cue

(use-package cue-mode
  :ensure t)

;;; Customization

(put 'narrow-to-region 'disabled nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#202020" :foreground "#F9F6F2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "nil" :family "Inconsolata"))))
 '(company-tooltip ((t (:background "#2e5077"))))
 '(company-tooltip-annotation ((t nil)))
 '(company-tooltip-common ((t (:foreground "#d73c3f"))))
 '(company-tooltip-common-selection ((t (:foreground "#921e20"))))
 '(company-tooltip-scrollbar-thumb ((t (:background "#7090ff"))))
 '(company-tooltip-scrollbar-track ((t (:background "#6080a0"))))
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
 '(font-lock-comment-delimiter-face ((t (:foreground "RoyalBlue1"))))
 '(font-lock-comment-face ((t (:foreground "RoyalBlue1"))))
 '(font-lock-warning-face ((t (:background "#202020" :foreground "#ff6523"))))
 '(git-gutter+-added ((t (:inherit font-lock-comment-face :weight bold))))
 '(git-gutter+-deleted ((t (:inherit font-lock-comment-face :weight bold))))
 '(git-gutter+-modified ((t (:inherit font-lock-comment-face :weight bold))))
 '(highlight ((t (:background "#4A6310"))))
 '(magit-diff-context-highlight ((t (:background "#312C33" :foreground "grey70"))))
 '(magit-diff-file-heading ((t (:background "#794214" :weight bold))))
 '(magit-diff-file-heading-highlight ((t (:inherit magit-section-highlight :background "#a55a1c"))))
 '(magit-diff-hunk-heading ((t (:background "#113f67" :foreground "grey90" :weight bold))))
 '(magit-diff-hunk-heading-highlight ((t (:background "#38598b"))))
 '(mu4e-view-body-face ((t (:background "gray10"))))
 '(org-block-begin-line ((t (:inherit org-meta-line :background "#102D3F" :foreground "#3A86B4" :slant normal))))
 '(org-hide ((t (:background "#18363E" :foreground "#18363E"))))
 '(org-indent ((t (:inherit org-hide :background "#18363E"))))
 '(org-level-1 ((t (:extend t :foreground "#DD517F" :inverse-video t :weight bold :height 1.5))))
 '(org-level-2 ((t (:inherit org-level-1 :extend nil :foreground "#acaec5" :inverse-video nil :overline t :weight bold :height 0.8))))
 '(org-level-3 ((t (:extend nil :foreground "#acaec5" :weight bold :height 1.1))))
 '(org-meta-line ((t (:inherit default :foreground "gray89"))))
 '(org-todo ((t (:inherit default :foreground "#AA9D94" :weight bold))))
 '(org-todo-blocked ((t (:inherit nil :background "gray95" :foreground "#C6808C" :inverse-video t))))
 '(org-todo-inprogress ((t (:inherit nil :background "gray99" :foreground "#3E88A5" :inverse-video t))))
 '(org-todo-ready ((t (:inherit nil :background "gray85" :foreground "#6B8E6E" :inverse-video t))))
 '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "#389AD2"))))
 '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "#60BBB7"))))
 '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "#B5C047"))))
 '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "#F6CD53"))))
 '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground "#EDA546"))))
 '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground "#E46B36"))))
 '(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face :foreground "#CA4640"))))
 '(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-base-face :foreground "#DD6D7C"))))
 '(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-base-face :foreground "#9A69D2"))))
 '(vertical-border ((t (:foreground "gray23")))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("679494c620fd0c8637c485748cd9a582a5646f66f261cdf6af10e1854111a2bb" default))
 '(evil-undo-system 'undo-tree)
 '(mouse-wheel-progressive-speed nil)
 '(org-jira-custom-jqls
   '((:jql " assignee = currentUser() and createdDate < '2019-01-01' order by created DESC " :limit 100 :filename "last-years-work")
     (:jql " assignee = currentUser() and createdDate >= '2019-01-01' order by created DESC " :limit 100 :filename "this-years-work")
     (:jql " (assignee = currentUser() OR reporter = currentUser()) and resolution = unresolved ORDER BY priority DESC, created ASC " :limit 100 :filename "all my issues")
     (:jql " (watcher = currentUser() AND NOT (assignee = currentUser() AND reporter = currentUser())) and resolution = unresolved ORDER BY priority DESC, created ASC " :limit 100 :filename "watched only")))
 '(safe-local-variable-values
   '((lsp-go-use-gofumpt . t)
     (gofmt-args "-local" "github.com/signalsciences/sigsci/")
     (lsp-go-goimports-local . "github.com/signalsciences/sigsci/")))
 '(warning-suppress-types '((lsp-mode) (comp))))

;; Mouse With a normal `setq', I always get prompted on exit to save
;; this value. This needs to be called after `custom-set-variables' and
;; `custom-set-faces' are called otherwise we erase those.
(customize-mark-as-set 'mouse-wheel-progressive-speed)

(defun diredp-mark-if-not-inode-in-other-directoy (other-dir)
  "E."
  (interactive "D")
  (let ((inode (file-attribute-inode-number (file-attributes (dired-get-file-for-visit) 'string))))
	(shell-command (format "find %s -inum %d" other-dir inode) )
	)
  )


;;; init.el ends here
