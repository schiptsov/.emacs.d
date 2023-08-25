;;; -*- mode: emacs-lisp; lexical-binding: t; coding: utf-8; -*-

(setq-default load-prefer-newer t)

;; temporary kludge
(setq-default user-emacs-directory "/home/lngnmn2/.nano-emacs.d/")

(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          '(lambda ()
             (setq gc-cons-threshold (* 16 1024 1024)
                   gc-cons-percentage 0.1)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; we use straight.el
(setq package-enable-at-startup nil)
(setq package-quickstart nil)

;; proper encodings for org-babel
;;
(setenv "LC_CTYPE" "UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LANG" "en_US.UTF-8")

(set-charset-priority 'unicode)
(set-language-environment 'UTF-8)
(set-default-coding-systems 'utf-8)

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(setq locale-coding-system 'utf-8
      coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; We use the 'straight bloatware
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
                (url-retrieve-synchronously
                 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
             'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

(setq straight-use-package-by-default t)
(setq straight-vc-git-default-clone-depth 1)

;; we will use this DSLs (a set of macros)
(setq use-package-always-defer t
      use-package-always-ensure t
      use-package-compute-statistics t
      use-package-verbose t)

;; use-package will use 'straight
(straight-use-package '(use-package :type built-in))
(straight-use-package '(bind-key :type built-in))
(straight-use-package 'diminish)
(straight-use-package 'delight)

(straight-use-package 'async)
(use-package ob-async
  :hook (org-load . (lambda () (require 'ob-async))))

(menu-bar-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode -1)

;; Use the latest version instead of the bundled one
(straight-use-package 'org)

;; stolen hacks from Doom Emacs
(defun doom-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

;;; Hooks
(defmacro add-transient-hook! (hook-or-function &rest forms)
  "Attaches a self-removing function to HOOK-OR-FUNCTION.

FORMS are evaluated once, when that function/hook is first invoked, then never
again.

HOOK-OR-FUNCTION can be a quoted hook or a sharp-quoted function (which will be
advised)."
  (declare (indent 1))
  (let ((append? (if (eq (car forms) :after) (pop forms)))
        (fn (gensym "doom-transient-hook")))
    `(let ((sym ,hook-or-function))
       (defun ,fn (&rest _)
         ,(format "Transient hook for %S" (doom-unquote hook-or-function))
         ,@forms
         (let ((sym ,hook-or-function))
           (cond ((functionp sym) (advice-remove sym #',fn))
                 ((symbolp sym)   (remove-hook sym #',fn))))
         (unintern ',fn nil))
       (cond ((functionp sym)
              (advice-add ,hook-or-function ,(if append? :after :before) #',fn))
             ((symbolp sym)
              (put ',fn 'permanent-local-hook t)
              (add-hook sym #',fn ,append?))))))

(add-transient-hook! #'org-babel-execute-src-block
  (require 'ob-async))

;; and a "literate" org-mode configuration
(setf config-org (expand-file-name "~/.nano-emacs.d/config.org"))
(when (file-readable-p config-org)
  (org-babel-load-file config-org))

;;; init.el ends here
