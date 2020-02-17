;;;; -*- lexical-binding:t -*-

;;; Use MELPA and use-package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-install 'use-package)

(eval-when-compile
  (require 'use-package))

;;; Add load-path to lisp packages part of my git repo.
(add-to-list 'load-path (file-name-directory load-file-name))

;;; Appearance

(setq-default default-frame-alist '((font . "Cousine:-11")))

;;; General

(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)

(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)
(setq-default tab-always-indent 'complete)
(setq-default require-final-newline t)

(setq-default sentence-end-double-space nil)
;; M-k to kill line from point to beginning of line. (Reverse of C-k)
(global-set-key (kbd "M-k")
                (lambda () (interactive) (kill-line 0)))

(use-package whitespace-cleanup-mode
  :ensure t
  :config
  (global-whitespace-cleanup-mode))

;;; Color Theme

(use-package solarized-theme
  :ensure t
  :init
  (setq solarized-distinct-fringe-background t)
  (setq solarized-high-contrast-mode-line t)
  (setq solarized-use-less-bold t)
  :config
  (load-theme 'solarized-light t)
  ;; Yuck. This has to use eval after changing the macro's signature:
  ;;   https://github.com/bbatsov/solarized-emacs/commit/b47d513aa4a452ae7875b65c3f7ee006444711c8
  ;; (eval `(solarized-with-color-variables 'light ,solarized-light-color-palette-alist
  ;;          (custom-theme-set-faces 'solarized-light
  ;;                                  `(font-lock-constant-face ((,class (:foreground ,blue)))))))
  )


;;; Window/Frame movement

;; Select windows via S-<left> / S-<right> / S-<up> / S-<down>
(use-package windmove
  :init
  (require 'framemove)
  (setq-default framemove-hook-into-windmove t)
  :config
  (windmove-default-keybindings))

;; `C-c left` restores previous window configuration
(use-package winner
  :config
  (winner-mode +1))

;; (use-package color-moccur
;;   :ensure t
;;   :commands (isearch-moccur isearch-all)
;;   :bind (("M-s O" . moccur)
;;          :map isearch-mode-map
;;          ("M-o" . isearch-moccur)
;;          ("M-O" . isearch-moccur-all))
;;   :init
;;   (setq isearch-lazy-highlight t)
;;   :config
;;   (use-package moccur-edit
;;     :ensure t))


;; Fn-o => ö, Fn-u => ü, Fn-a => ä
;; (global-set-key (kbd "<kp-6>")
;;                 (lambda () (interactive) (insert "ö")))
;; (global-set-key (kbd "<XF86Launch3>")
;;                 (lambda () (interactive) (insert "ä")))
;; (global-set-key (kbd "<kp-4>")
;;                 (lambda () (interactive) (insert "ü")))

;;; Emacs server

(server-start)

;;; Man

(setq-default Man-switches "-a")

;;; Regexp replace

(use-package visual-regexp
  :ensure t
  :bind ("M-r" . vr/query-replace))

;;; Shell

(autoload 'bash-completion-dynamic-complete
  "bash-completion"
  "BASH completion hook")

(add-hook 'shell-dynamic-complete-functions
          'bash-completion-dynamic-complete)

;;; Eshell

(dolist (h '(shell-mode-hook eshell-mode-hook))
  (add-hook h (lambda ()
                (setq truncate-lines nil))))

;;; Company Mode

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (add-hook 'company-mode-hook (lambda ()
                                 (add-hook 'completion-at-point-functions
                                           (lambda ()
                                             (lambda () (company-complete)))
                                           nil
                                           t))))

;;; which-key

(use-package which-key
  :ensure t
  :config
  (setq-default which-key-idle-delay 0.85)
  (setq-default which-key-idle-secondary-delay 0.5)
  (which-key-mode 1))

;;; Lice

(use-package lice
  :ensure t
  :config
  (setq-default lice:default-license "mit")
  (setq-default lice:copyright-holder
                "Tobias Rittweiler <trittweiler+opensource@gmail.com>"))

;;; with-editor

;; (use-package with-editor
;;   :ensure t
;;   :init
;;   (add-hook 'shell-mode-hook  'with-editor-export-editor)
;;   (add-hook 'term-exec-hook   'with-editor-export-editor)
;;   (add-hook 'eshell-mode-hook 'with-editor-export-editor))

;;; Slime

(use-package slime
  :ensure t
  :config
  (setq slime-lisp-implementations
        '((sbcl ("/usr/local/bin/sbcl"))
          (ccl  ("/home/trittweiler/software/ccl-1.11.5/bin/ccl"))))
  (setq slime-default-lisp 'sbcl)
  (setq slime-contribs '(slime-fancy
                         slime-asdf
                         slime-sbcl-exts
                         slime-highlight-edits)))

;;; Paredit

(use-package paredit
  :ensure t
  :hook ((lisp-mode emacs-lisp-mode) . paredit-mode))

;;; Eldoc & Macrostep

(use-package eldoc
  :hook ((python-mode c-mode-common emacs-lisp-mode) . eldoc-mode))

(use-package macrostep
  :ensure t)

;;; Flycheck

(use-package flycheck
  :ensure t
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error))
  :init
  (add-hook 'emacs-lisp-mode-hook #'flycheck-mode)
  (add-hook 'protobuf-mode-hook #'flycheck-mode)
  :config
  (setq-default flycheck-check-syntax-automatically
                '(save idle-change mode-enabled))
  (setq-default flycheck-idle-change-delay 4))


(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode .  modern-c++-font-lock-mode))

(use-package google-c-style
  :ensure t
  :init
  (add-hook 'c-mode-hook
            (lambda ()
              (c-add-style "Google" google-c-style t)))
  (add-hook 'c++-mode-hook
            (lambda ()
              (c-add-style "Google" google-c-style t))))


;; ;;; Buck build system

;; (eval-when-compile (require 'cl-lib))   ; cl-assert

;; (defun sol--update-flycheck-include-dirs ()
;;   (let ((dirs (sol--collect-generated-include-dirs (buffer-file-name))))
;;     (setq-local flycheck-clang-include-path
;;                 (append flycheck-clang-include-path dirs))
;;     (setq-local flycheck-gcc-include-path
;;                 (append flycheck-clang-include-path dirs))))

;; (defun sol--collect-generated-include-dirs (&optional filename)
;;   (let ((buck-out-directory (sol--find-buck-root filename)))
;;     (when buck-out-directory
;;       (directory-files-recursively (concat buck-out-directory "gen")
;;                                    "^include$\\|^.*#.*,.*headers.*$"
;;                                    t))))

;; (defun sol--find-file-upwards (needle &optional directory levels)
;;   (cl-assert (equal (file-name-nondirectory needle) needle))
;;   (unless directory
;;     (setq directory (file-name-directory
;;                      (directory-file-name (buffer-file-name)))))
;;   (setq levels (or levels 15))
;;   (cond ((<= levels 0) nil)
;;         (t
;;          (let ((candidate (concat directory needle)))
;;            (cond ((file-exists-p candidate) candidate)
;;                  (t
;;                   (sol--find-file-upwards needle
;;                                           (file-name-directory
;;                                            (directory-file-name directory))
;;                                           (1- levels))))))))

;; (defvar-local sol--buck-root-cached nil)

;; (defun sol--find-buck-root (&optional filename levels)
;;   (if (and sol--buck-root-cached
;;            (file-exists-p sol--buck-root-cached))
;;       sol--buck-root-cached
;;     (let ((it (sol--find-file-upwards "buck-out" filename levels)))
;;       (when it
;;         (setq-local sol--buck-root-cached
;;                     (file-name-as-directory it))))))

;; ;;; From https://github.com/lunaryorn/old-emacs-configuration/blob/master/lisp/flycheck-virtualenv.el (GPL3)
;; (defun flycheck-virtualenv-executable-find (executable)
;;   "Find an EXECUTABLE in the current virtualenv if any."
;;   (if (bound-and-true-p python-shell-virtualenv-root)
;;       (let ((exec-path (python-shell-calculate-exec-path)))
;;         (executable-find executable))
;;     (executable-find executable)))

;; (defun sol--update-virtualenv-path ()
;;   (let ((buck-out-directory (sol--find-buck-root (buffer-file-name))))
;;     (when buck-out-directory
;;       (let ((candidate (concat buck-out-directory "gen/common/python/venv/")))
;;         (when (file-exists-p candidate)
;;           (setq-local python-shell-virtualenv-root candidate))))))

;;; Projectile

(use-package projectile
  :ensure t
  :demand t       ; always load for (featurep 'projectile) in lsp-mode
  :bind-keymap
  ("C-c p" . projectile-command-map))


;;; LSP

(use-package ccls
  :ensure t)

(use-package lsp-mode
  :ensure t
  :after (ccls)
  :config
  (setq-default lsp-log-io t)
  (setq-default lsp-auto-guess-root t)
  (setq-default lsp-prefer-capf t)                  ; we do our own setup
  (setq-default lsp-enable-completion-at-point nil) ;  for company-mode
  (setq-default lsp-prefer-flymake t)
  (setq-default lsp-enable-xref t)
  (setq-default lsp-eldoc-render-all nil)
  (setq-default lsp-eldoc-hook '(trittweiler:lsp-eldoc))
  (setq-default lsp-signature-auto-activate nil)
  (setq-default lsp-signature-render-documentation nil)
  (setq-default lsp-clients-clangd-args '("-background-index"))
  (use-package company-lsp)
  :init
  (require 'ccls)
  (add-hook 'c++-mode-hook #'lsp)
  (add-hook 'c-mode-hook #'lsp)
  (add-hook 'scala-mode-hook #'lsp)
  (add-hook 'python-mode-hook #'lsp))

(defun trittweiler:lsp-eldoc ()
  "Display signature info (based on `textDocument/signatureHelp')"
  (lsp-request-async "textDocument/signatureHelp"
                     (lsp--text-document-position-params)
                     #'(lambda (signature)
                         (let ((message (lsp--signature->message signature)))
                           (when (s-present? message)
                             (message "[%s]" message)
                             (lsp--eldoc-message
                              ;; Remove those pesky "1/1 "
                              (replace-regexp-in-string "^[0-9/|│ ]*" "" message)))))
                     :cancel-token :signature))



(use-package company-lsp
  :ensure t
  :commands company-lsp
  :config
  (add-to-list 'company-backends 'company-lsp))

(use-package flymake
  :ensure t
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))


;;; Scala

(use-package scala-mode
  :ensure t
  :mode "\\.s\\(cala\\|bt\\)$"
  :config
  (setq-default scala-indent:align-parameters t)
  (setq-default scala-indent:align-forms t)
  )

(use-package sbt-mode
  :ensure t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))


;;; Python

;; (use-package elpy
;;   :ensure t)


;;; GDB

(setq-default gdb-many-windows t)


;;; Misc modes

(use-package bazel-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "markdown"))

(use-package protobuf-mode
  :ensure t)

(use-package systemd
  :ensure t)

(use-package magit
  :ensure t
  :bind (("s-g" . magit-file-dispatch))
  :init
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
  (setq-default flyspell-issue-message-flag nil))
