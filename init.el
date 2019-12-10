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

;; Modeline

(column-number-mode 1)


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
