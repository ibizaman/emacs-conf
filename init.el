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

(use-package evil
  :straight t
  :config
  (evil-mode 1))

(use-package flycheck
  :straight t
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save idle-change)
	flycheck-relevant-error-other-file-show nil))

(use-package flyspell
  :straight t
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
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package magit
  :straight t
  :bind (("C-x g" . magit))
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1
	magit-published-branches nil))

(use-package evil-magit
  :straight t
  :after (magit evil))


; UI

(menu-bar-mode 0)
(tool-bar-mode 0)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))

(set-face-attribute 'default nil :font "Inconsolata-g-10")

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


; Helper packages

(setq sentence-end-double-space nil)

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
	org-structure-template-alist)

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
;; Haskell

(use-package haskell-mode
  :straight t
  :config
  (setq haskell-hoogle-url "http://localhost:65000/?hoogle=%s"
	haskell-mode-hook '(capitalized-words-mode haskell-decl-scan-mode haskell-indent-mode haskell-indentation-mode)
	haskell-mode-stylish-haskell-path "~/.local/bin/brittany"
	haskell-stylish-on-save t))

(use-package intero
  :straight t
  :config
  (intero-global-mode 1))
