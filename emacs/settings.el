;;;; -*- lexical-binding:t -*-

;;; Use MELPA and use-package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(eval-when-compile
  (require 'use-package))

;;; Appearance

(setq-default default-frame-alist '((font . "Cousine:-11")))

;;; General

(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)

(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)

;; M-k to kill line from point to beginning of line. (Reverse of C-k)
(global-set-key (kbd "M-k")
                (lambda () (interactive) (kill-line 0)))

;; Select windows via S-<left> / S-<right> / S-<up> / S-<down>
(use-package windmove
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
  :hook (lisp-mode-hook emacs-lisp-mode-hook))

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
  (add-hook 'c-mode-hook
            (lambda ()
              (setq flycheck-clang-language-standard "c11")))
  (add-hook 'c++-mode-hook
            (lambda ()
              (setq flycheck-clang-language-standard "c++14")))
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


;;; Markdown

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "markdown"))


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
