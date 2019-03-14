;;;; -*- lexical-binding:t -*-

;;; Use MELPA and use-package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

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

;;; Color Theme

(use-package solarized-theme
  :ensure t
  :init
  (setq solarized-distinct-fringe-background t)
  (setq solarized-high-contrast-mode-line t)
  :config
  (load-theme 'solarized-light t))

;;; Window/Frame movement

;; Select windows via S-<left> / S-<right> / S-<up> / S-<down>
(use-package windmove
  :init
  (require 'framemove)
  (setq-default framemove-hook-into-windmove t)
  :config
  (windmove-default-keybindings))

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


;;; Man

(setq-default Man-switches "-a")

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
  (add-hook 'after-init-hook 'global-company-mode))

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
  (add-hook 'c-mode-common-hook #'flycheck-mode)
  (add-hook 'python-hook #'flycheck-mode)
  (add-hook 'emacs-lisp-mode-hook #'flycheck-mode)
  (add-hook 'c-mode-hook
            (lambda ()
              (setq flycheck-clang-language-standard "c11")
              (setq flycheck-gcc-language-standard "c11")
              (add-hook 'flycheck-before-syntax-check-hook
                        'sol--update-flycheck-include-dirs)))
  (add-hook 'c++-mode-hook
            (lambda ()
              (setq flycheck-clang-language-standard "c++17")
              (setq flycheck-gcc-language-standard "c++17")
              (add-hook 'flycheck-before-syntax-check-hook
                        'sol--update-flycheck-include-dirs)))
  :config
  (setq-default flycheck-clang-pedantic t)
  (setq-default flycheck-clang-warnings
                '("everything"
                  "no-c++98-compat"
                  "no-c++98-compat-pedantic"
                  "no-exit-time-destructors"
                  "no-global-constructors"
                  "no-missing-prototypes"
                  "no-unused-macros"
                  "no-weak-vtables"))
  )


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


;;; Buck build system

(eval-when-compile (require 'cl-lib))   ; cl-assert

(defun sol--update-flycheck-include-dirs ()
  (let ((dirs (sol--collect-generated-include-dirs (buffer-file-name))))
    (setq-local flycheck-clang-include-path
                (append flycheck-clang-include-path dirs))
    (setq-local flycheck-gcc-include-path
                (append flycheck-clang-include-path dirs))))

(defun sol--collect-generated-include-dirs (&optional filename)
  (let ((buck-out-directory (sol--find-buck-out filename)))
    (when buck-out-directory
      (directory-files-recursively (concat buck-out-directory "gen")
                                   "^include$\\|^.*#.*,.*headers.*$"
                                   t))))

(defun sol--find-file-upwards (needle &optional directory levels)
  (cl-assert (equal (file-name-nondirectory needle) needle))
  (unless directory
    (setq directory (file-name-directory
                     (directory-file-name (buffer-file-name)))))
  (setq levels (or levels 6))
  (cond ((<= levels 0) nil)
        (t
         (let ((candidate (concat directory needle)))
           (cond ((file-exists-p candidate) candidate)
                 (t
                  (sol--find-file-upwards needle
                                          (file-name-directory
                                           (directory-file-name directory))
                                          (1- levels))))))))

(defun sol--find-buck-out (&optional filename levels)
  (let ((it (sol--find-file-upwards "buck-out" filename levels)))
    (when it
      (file-name-as-directory it))))

;;; RTags

(use-package rtags
  :ensure t)


;;; Misc

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "markdown"))

(use-package meson-mode
  :ensure t
  :hook (meson-mode-hook . company-mode))

(use-package systemd
  :ensure t)

;; GDB

(setq-default gdb-many-windows t)

;;; Custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (use-package strace-mode flymd gh-md markdown-mode markdown-mode+ markdown-preview-eww elf-mode elfeed elfeed-goodies elfeed-org jdee cmake-mode google-c-style multiple-cursors modern-cpp-font-lock ac-rtags company-rtags flycheck-rtags helm-rtags ivy-rtags rtags lice org which-key ggtags browse-kill-ring bash-completion flycheck flycheck-mix git git-commit magit magit-imerge paredit slime))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
