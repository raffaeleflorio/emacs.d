;; load-path region
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

(add-to-list 'load-path site-lisp-dir)
(dolist (repo (directory-files site-lisp-dir t "^\\w"))
  (if (file-directory-p repo)
      (add-to-list 'load-path (cond ((equal repo (concat site-lisp-dir "/magit")) (concat repo "/lisp"))
                                    ((equal repo (concat site-lisp-dir "/geiser")) (concat repo "/elisp"))
                                    (t repo)))))

;; require and load region
(load "magit-autoloads")
(require 'avy)
(require 'smex)
(require 'ivy)
(require 'swiper)
(require 'counsel)
(require 'expand-region)
(require 'geiser)
(require 'lispy)

;; ui / ux region
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(add-to-list 'default-frame-alist
             '(font . "Inconsolata-11:style=Medium"))

;; keybinding region
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "M-:") 'avy-goto-word-1)
(global-set-key (kbd "C-'") 'ivy-avy)
(define-key lispy-mode-map-lispy (kbd "M-o") nil)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-ò") 'er/expand-region)

;; ivy region
(ivy-mode 1)

;; geiser region
(if (not (file-exists-p (concat user-emacs-directory "geiser")))
    (make-directory (concat user-emacs-directory "geiser")))
(setq geiser-repl-history-filename (concat user-emacs-directory "geiser/history")
      geiser-repl-query-on-kill-p nil
      gesier-repl-skip-version-check-p t
      geiser-guile-binary "guile2.2"
      geiser-active-implementations '(guile))

;; dired region
(setq dired-recursive-deletes 'always
      dired-recursive-copies 'top)

;; prompt region
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil
      ido-create-new-buffer 'always
      kill-buffer-query-functions (remq 'process-kill-buffer-query-function
                                        kill-buffer-query-functions))

;; startup screen region
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-buffer-choice 'eshell)

;; base programming mode hook
(defun base-programming-hook ()
  (linum-mode 1)
  (hl-line-mode 1)
  (column-number-mode)
  (show-paren-mode)
  (setq show-paren-delay 0
        indent-tabs-mode nil))

;; js-mode region
(defun my-js-mode-hook ()
  (base-programming-hook)
  (setq js-indent-level 4))

(add-hook 'js-mode-hook 'my-js-mode-hook)

;; emacs-lisp-mode region
(defun my-emacs-lisp-mode-hook ()
  (base-programming-hook)
  (lispy-mode 1))

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

;; scheme-mode region
(defun my-scheme-mode-hook ()
  (base-programming-hook)
  (lispy-mode 1))

(add-hook 'scheme-mode-hook 'my-scheme-mode-hook)

;; sh-mode region
(defun my-sh-mode-hook ()
  (base-programming-hook)
  (setq sh-basic-offset 8))

(add-hook 'sh-mode-hook 'my-sh-mode-hook)

;; c-mode region
(defun my-c-mode-hook ()
  (base-programming-hook)
  (setq c-basic-offset 8
        c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "linux"))))

(add-hook 'c-mode-hook 'my-c-mode-hook)
