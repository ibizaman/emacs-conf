; Install straight

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


; Base packages

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
  :delight undo-tree-mode)

(use-package autorevert
  :delight auto-revert-mode)

(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)  ; needed for evil-collection
  (setq evil-want-keybinding nil) ; needed for evil-collection
  :config
  (evil-mode 1)
  (evil-define-key 'normal Info-mode-map (kbd "]") #'Info-forward-node)
  (evil-define-key 'normal Info-mode-map (kbd "[") #'Info-backward-node)
  (evil-define-key 'normal 'global "gt" 'counsel-semantic-or-imenu))

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
	 log-edit
	 log-view
	 mu4e
	 occur
	 simple
	 which-key))
  (evil-collection-occur-setup)
  )

(use-package flycheck
  :straight t
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save idle-change)
	flycheck-relevant-error-other-file-show nil))

(use-package yasnippet
  :straight t)

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
  (add-hook 'after-init-hook 'global-company-mode))

(use-package magit
  :straight t
  :bind (("C-x g g" . magit-status)
		 ("C-x g d" . magit-diff-buffer-file)
		 ("C-x g b" . magit-blame)
		 ("C-x g l" . magit-log)
		 :map magit-file-mode-map
		 ("C-x g" . nil)
         :map magit-mode-map
		 ("o" . magit-visit-thing)
		 ("O" . magit-diff-visit-file-other-window))
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1
		magit-published-branches nil)
  (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-assigned-pullreqs nil t)
  (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-assigned-issues nil t)
  (evil-define-key 'normal magit-mode-map (kbd "o") 'magit-visit-thing)
  (evil-define-key 'normal magit-mode-map (kbd "O") 'magit-diff-visit-file-other-window))

(use-package evil-magit
  :straight t
  :after (magit evil))

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
  :bind (("C-s" . 'swiper-thing-at-point))
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
  (projectile-mode 1))

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

; UI

(menu-bar-mode 0)
(tool-bar-mode 0)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))

(if (x-list-fonts "*-Inconsolata-*")
    (set-face-attribute 'default nil :font "Inconsolata-13")
  (set-face-attribute 'default nil :font "InconsolataG-10"))

(setq kill-do-not-save-duplicates t
      ; From https://stackoverflow.com/a/29092845/1013628
      select-enable-clipboard nil)

(setq-default tab-width 4)
(defun my/disable-tabs ()
  "Disable tabs and set them to 4 spaces."
  (setq-default tab-width 4)
  (setq tab-width 4)
  (setq indent-tabs-mode nil))
; Tabs are used to format buffer with `lsp-format-buffer'.
(add-hook 'haskell-mode-hook 'my/disable-tabs)

;; Modeline

(column-number-mode 1)

;; Theme

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


; Miscellaneous

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


; Helper functions
(defun get-secret (host user)
  (funcall (plist-get (nth 0 (auth-source-search :host host :user user :max 1)) :secret)))

;; Shortcuts for jumping around functions
(progn
  (defun ibizaman/jump-functions()
	"Add bindings to move around functions."
    (define-key prog-mode-map (kbd "C-k") 'beginning-of-defun)
    (define-key prog-mode-map (kbd "C-j") 'end-of-defun))

  (add-hook 'prog-mode-hook 'ibizaman/jump-functions))


; Helper packages

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


; Org

(use-package org
  :straight t
  :after (evil es-mode)
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
     (dot . t)))

  (org-babel-lob-ingest "~/.emacs.d/emacs-lob.org")

  (progn
    (defun ibizaman/org-copy-element ()
      (interactive)
      (let* ((elem (org-element-at-point))
             (beg (org-element-property :begin elem))
             (end (org-element-property :end elem)))
        (copy-region-as-kill beg end)
        (goto-char end))))

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
        '((:results . "silent")
          (:terminal . "iterm")))
  (setq org-babel-tmux-session-prefix "ob-"))


; Language specific packages

(global-set-key (kbd "C-c C-j") 'find-function)

;; Language Server Protocol

(defun my/lsp-format-buffer-silent ()
  "Silence errors from `lsp-format-buffer'."
  (ignore-errors (lsp-format-buffer)))

(use-package lsp-mode
  :straight t
  :commands lsp
  :hook ((sh-mode . lsp-deferred)
		 (javascript-mode . lsp-deferred)
		 (html-mode . lsp-deferred)
		 (before-save . my/lsp-format-buffer-silent))
  :config
  (setq lsp-signature-auto-activate t)
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

;; Elisp

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

;; Haskell

(use-package haskell-mode
  :straight t
  :hook ((haskell-mode . capitalized-words-mode)
		 (haskell-mode . haskell-decl-scan-mode)
		 (haskell-mode . haskell-indent-mode)
		 (haskell-mode . haskell-indentation-mode))
  :config
  (setq haskell-hoogle-url "http://localhost:65000/?hoogle=%s"
	haskell-mode-stylish-haskell-path "~/.local/bin/brittany"
	haskell-stylish-on-save t))

(use-package lsp-haskell
  :straight t
  :hook (haskell-mode . lsp-deferred)
  :init
  (setq lsp-prefer-flymake nil)
  (require 'lsp-haskell))

;; Markdown

(use-package markdown-mode
  :straight t
  :commands (markdown-mode gf-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Yaml

(use-package yaml-mode
  :straight t
  :hook (yaml-mode . lsp-deferred))

;; Go

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


; Elfeed

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
