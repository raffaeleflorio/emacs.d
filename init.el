;; load-path region
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

(add-to-list 'load-path site-lisp-dir)
(dolist (repo (directory-files site-lisp-dir t "^\\w"))
  (if (file-directory-p repo)
      (if (equal repo (concat site-lisp-dir "/magit"))
          (add-to-list 'load-path (concat repo "/lisp"))
        (add-to-list 'load-path repo))))

;; require and load region
(load "magit-autoloads")
(require 'avy)
(require 'smex)
(require 'ivy)
(require 'swiper)
(require 'counsel)
(require 'expand-region)

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
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-ò") 'er/expand-region)

;; ivy region
(ivy-mode 1)

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
  (setq-default indent-tabs-mode nil))

;; js-mode region
(defun my-js-mode-hook ()
  (base-programming-hook)
  (setq show-paren-delay 0)
  (show-paren-mode)
  (setq js-indent-level 4))

(add-hook 'js-mode-hook 'my-js-mode-hook)

;; emacs-lisp-mode region
(defun my-emacs-lisp-mode-hook ()
  (base-programming-hook))

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

;; sh-mode region
(defun my-sh-mode-hook ()
  (base-programming-hook)
  (setq sh-basic-offset 8))

(add-hook 'sh-mode-hook 'my-sh-mode-hook)
