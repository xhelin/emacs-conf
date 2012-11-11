(add-to-list 'load-path "~/.emacs.d")
(setq make-backup-files nil)
(setenv "LD_LIBRARY_PATH" (concat "/usr/local/lib:" (getenv "LD_LIBRARY_PATH")))
(load-file (expand-file-name "~/.emacs.d/parse-cflags.el"))

(global-set-key (kbd "C-SPC") nil)


(setq-default show-trailing-whitespace t)

;;; package
(require 'package)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

(defun require-package (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

;;; pre-installed packages
(require-package 'undo-tree)
(require-package 'idomenu)
(require-package 'smex)
(require-package 'dired+)
(require-package 'auto-complete)
(require-package 'yasnippet)

;;; using c-c c-v c-x c-z keybindings
(cua-mode 1)

;; unto tree mode
(global-undo-tree-mode 1)

(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'redo)

;;; copy or cut line shortcut
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy the current line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (progn
       (message "Current line is copied.")
       (list (line-beginning-position) (line-beginning-position 2)) ) ) ))

(defadvice kill-region (before slick-copy activate compile)
  "When called interactively with no active region, cut the current line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (progn
       (list (line-beginning-position) (line-beginning-position 2)) ) ) ))

;;; line number, column number
(global-linum-mode 1)
(column-number-mode 1)

;; ibus
;;;(require 'ibus)
;; Turn on ibus-mode automatically after loading .emacs
;;;(add-hook 'after-init-hook 'ibus-mode-on)
;; Use C-SPC for Set Mark command
;;;(ibus-define-common-key ?\C-\s nil)
;; Use C-/ for Undo command
;;;(ibus-define-common-key ?\C-/ nil)
;; Change cursor color depending on IBus status
;;;(setq ibus-cursor-color '("red" "blue" "limegreen"))
;; Use s-SPC to toggle input status
;;;(global-set-key (kbd "s-SPC") 'ibus-toggle)

;;; org-mode
(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING(i!)" "HANGUP(h!)" "|" "DONE(d!)" "CANCEL(c!)")))

;;; c++ indentation
(defun my-c-common-hook ()
  (setq c-basic-offset 2)
  (c-set-offset 'substatement-open 0)
  (setq indent-tabs-mode nil)
  (set-local-clang-flags)
  (define-key c-mode-map [(meta ?/)] 'ac-complete-clang-async)
  (define-key c++-mode-map [(meta ?/)] 'ac-complete-clang-async)
)

(add-hook 'c-mode-common-hook 'my-c-common-hook)

;;; use c++-mode to open .h files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;; auto-complete
(require 'auto-complete-config)
(require 'auto-complete-clang-async)

(defun ac-cc-mode-setup ()
  (setq ac-clang-complete-executable "~/.emacs.d/clang-complete")
  (setq ac-sources '(ac-source-clang-async))
  (ac-clang-launch-completion-process)
)

(defun my-ac-config ()
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

(my-ac-config)

;;; ido and smex
(require 'ido)
(ido-mode t)
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


;;; yasnippet
;;(add-to-list 'load-path
;;	     "~/.emacs.d/elpa/yasnippet-20120819")
(require 'yasnippet)
(yas-global-mode 1)
;;(yas/load-directory "~/.emacs.d/elpa/yasnippet-20120819/snippets")
(global-set-key (kbd "C-c y") 'yas/expand)


;;; folding

(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'ess-mode-hook        'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)

(global-set-key [f1] 'hs-toggle-hiding)

;;; python
;;;(load "python-mode.el")
;;;(setenv "PYMACS_PYTHON" "python2.7")
;;;(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
;;;(setq interpreter-mode-alist (cons '("python" . python-mode) interpreter-mode-alist))
;;;(autoload 'python-mode "python-mode" "Python editing mode." t)
;;;(global-font-lock-mode t)
;;;(setq font-lock-maximum-decoration t)

;;;(ac-ropemacs-initialize)
;;;(pymacs-load "ropemacs" "rope-")
;;;(setq ropemacs-enable-autoimport t)

;;;(require 'pycomplete)



(setq ac-clang-flags
      (mapcar (lambda (item)(concat "-I" item))
              (split-string
              "
/usr/include/c++/4.6
/usr/include/c++/4.6/x86_64-linux-gnu/.
/usr/include/c++/4.6/backward
/usr/lib/gcc/x86_64-linux-gnu/4.6/include
/usr/local/include
/usr/lib/gcc/x86_64-linux-gnu/4.6/include-fixed
/usr/include/x86_64-linux-gnu
/usr/include
              "
               )))

















;;;(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;;; '(ansi-color-names-vector ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
;;; '(custom-enabled-themes (quote (manoj-dark))))
;;;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;;; )
;;;(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;;; '(custom-enabled-themes (quote (tango-dark))))
;;;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;;; )


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
