;;;; -*- lexical-binding:t -*-

;;; Use MELPA and use-package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(unless (package-installed-p 'use-package)
  (package-initialize)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'rx)

;;; Add load-path to lisp packages part of my git repo.
(when load-file-name
  (add-to-list 'load-path (file-name-directory load-file-name)))

;;; If Emacs was installed locally, add the standard site-lisp
;;; directory because the OS packages may have placed emacs stuff
;;; there. E.g. cmake-mode.
(dolist (dir (cons "/usr/share/emacs/site-lisp"
                   (directory-files "/usr/share/emacs/site-lisp"
                                    t nil t)))
  (when (and (file-accessible-directory-p dir)
             (not (string-match-p (rx "/" (or "." "..") eol) dir)))
    (add-to-list 'load-path dir)))


;;; Appearance

(set-frame-font "Cousine 10" nil t)

;; Color Theme

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

;;; General

(tool-bar-mode -1)
(when window-system
  (scroll-bar-mode -1))
(column-number-mode 1)

(setq-default load-prefer-newer t)
(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)
(setq-default tab-always-indent 'complete)
(setq-default require-final-newline t)
(setq-default sentence-end-double-space nil)

(let ((programming-mode-hooks
       '(c-mode-common-hook python-mode-hook lisp-mode-hook emacs-lisp-mode-hook)))
  (dolist (h programming-mode-hooks)
    (add-hook h (lambda ()
                  (setq-local show-trailing-whitespace t)))))

(use-package whitespace-cleanup-mode
  :ensure t
  :config
  (global-whitespace-cleanup-mode))


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

;;; Some custom key bindings

(use-package find-file
  :bind (("C-x C-o" . 'ff-find-other-file))
  :init
  (setq-default ff-always-in-other-window t))

;; M-k to kill line from point to beginning of line. (Reverse of C-k)
(global-set-key (kbd "M-k")
                (lambda () (interactive) (kill-line 0)))

;; Fn-o => ö, Fn-u => ü, Fn-a => ä
(global-set-key (kbd "<kp-6>")
                (lambda () (interactive) (insert "ö")))
(global-set-key (kbd "<XF86Launch3>")
                (lambda () (interactive) (insert "ä")))
(global-set-key (kbd "<kp-4>")
                (lambda () (interactive) (insert "ü")))

;;; Emacs server

(when window-system
  (server-start))

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
  (add-hook 'company-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions
                        (lambda () #'company-complete)
                        nil
                        t))))

(defun trittweiler:dired-buffer-p (name)
  "Return non-nil if STR names a Dired buffer."
  (let ((buf (get-buffer name)))
    (when buf
      (eq (buffer-local-value 'major-mode buf) 'dired-mode))))

(use-package ivy
  :ensure t
  :bind (:map ivy-minibuffer-map
          ("TAB" . ivy-partial))
  :config
  (setq-default ivy-use-virtual-buffers t)
  (setq-default ivy-on-del-error-function nil)
  (setq-default ivy-count-format "(%d/%d) ")
  (add-to-list 'ivy-ignore-buffers #'trittweiler:dired-buffer-p)
  :init
  (setq-default enable-recursive-minibuffers t)
  (ivy-mode 1))

(use-package swiper
  :after (ivy)
  :ensure t
  :bind ("C-s" . swiper))

;;; which-key

;; (use-package which-key
;;   :ensure t
;;   :config
;;   (setq-default which-key-idle-delay 0.85)
;;   (setq-default which-key-idle-secondary-delay 0.5)
;;   (which-key-mode 1))

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
        '((sbcl ("/home/trittweiler/software/sbcl/bin/sbcl"))
          (ccl  ("/home/trittweiler/software/ccl-1.11.5/bin/ccl"))))
  (setq slime-default-lisp 'sbcl)
  (setq slime-contribs '(slime-fancy
                         slime-asdf
                         slime-sbcl-exts
                         slime-highlight-edits)))

;;; Paredit

(use-package paredit
  :ensure t
  :hook ((lisp-mode emacs-lisp-mode) . paredit-mode)
  :bind (:map paredit-mode-map
         ("M-?" . xref-find-references)))

;;; Eldoc & Macrostep

(use-package eldoc
  :hook ((python-mode c-mode-common emacs-lisp-mode) . eldoc-mode))

(use-package macrostep
  :ensure t
  :bind (:map emacs-lisp-mode-map
         ("C-c C-m" . macrostep-expand))
  :config
  (setq-default macrostep-expand-in-separate-buffer t))



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

;; Make M-Backspace work on CamelCase
(use-package subword-mode
  :hook c-mode
  :hook c++-mode
  :hook java-mode)

(use-package eglot
  :demand t
  :ensure nil
  :pin manual                           ; ~/src/open-source/eglot.git
  :init
  (add-to-list 'load-path
               (expand-file-name "~/src/open-source/elisp/eglot.git/"))
  :config
  (add-to-list 'eglot-server-programs
               '((c++-mode c-mode) . ("clangd"
                                      "--background-index"
                                      "--log=info" "--pretty"
                                      "--clang-tidy"
                                      )))
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyls" "-v")))
  :hook ((python-mode c-mode-common) . eglot-ensure))


(use-package flymake
  :ensure t
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))


;;; GDB

(setq-default gdb-many-windows t)


;;; Misc modes

(use-package cmake-mode
  :pin manual)                     ; comes from Ubuntu `cmake' package

(use-package bazel-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "markdown"))

(use-package vmd-mode
  :ensure t)

(use-package protobuf-mode
  :ensure t)

(use-package systemd
  :ensure t)

(use-package magit
  :ensure t
  :bind (("s-g" . magit-file-dispatch))
  :init
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
  (setq-default flyspell-issue-message-flag nil)
  (setq-default magit-diff-refine-hunk 'all))

(use-package org
  :ensure t
  :bind (:map org-mode-map
          ("S-<left>"  . windmove-left)
          ("S-<right>" . windmove-right)
          ("S-<up>"    . windmove-up)
          ("S-<down>"  . windmove-down)))

;;;

(provide 'settings)
