;; Basic initialization

(setq inhibit-startup-message t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)  ;; Give some breathing room


;;set up the visible bell
(setq visible-bell t)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;-------------------------Additional/Custom--------------------------------

(show-paren-mode)

(electric-pair-mode) ;; automatically close a brace as you open it

(global-hl-line-mode t) ;; highlight the current line

(ido-mode t) ;; auto-completion

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-use-fuzzy nil)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(backup-directory-alist (quote (("." . "~/.local/share/emacs/backups"))))
 '(compilation-context-lines 2)
 '(compilation-error-screen-columns nil)
 '(compilation-scroll-output t)
 '(compilation-search-path (quote (nil "src")))
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes (quote (tango-dark)))
 '(electric-indent-mode nil)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(line-move-visual t)
 '(next-error-highlight t)
 '(next-error-highlight-no-select t)
 '(next-line-add-newlines nil)
 '(ocamlformat-enable (quote enable))
 '(package-selected-packages
   (quote
    (xcscope helm eglot irony-eldoc flycheck-irony ac-clang ac-c-headers yasnippet irony company-irony company TAB ggtags use-package ocamlformat auto-complete)))
 '(require-final-newline t)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(visible-bell t))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:inherit (shadow default) :foreground "yellow")))))

;; set linum mode
(global-linum-mode)
;(defun my-linum-mode ()
;  (if buffer-file-name
;      (linum-mode)))
;(define-globalized-minor-mode my-global-linum-mode linum-mode my-linum-mode)
;(my-global-linum-mode)
; distance between number and code
(setq linum-format "%4d ")


;; ANSI color in compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Some key bindings

(global-set-key [f3] 'next-match)
(defun prev-match () (interactive nil) (next-match -1))
(global-set-key [(shift f3)] 'prev-match)
;(global-set-key [backtab] 'auto-complete)
;(global-set-key [()] 'completion-at-point)


;;-------------------Theme-------------------

;; themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(load-theme 'ample-zen t t)
(load-theme 'ample-flat t t)
;;(load-theme 'ample-light t t)
;; choose one to enable
;;(enable-theme 'ample-zen)
(enable-theme 'ample-flat)
;;(enable-theme 'ample-light)

(with-eval-after-load "ample-flat"
  ;; add one of these blocks for each of the themes you want to customize
  (custom-theme-set-faces
    'ample
    ;; this will overwride the color of strings just for ample-theme
    '(font-lock-string-face ((t (:foreground "#c5e1fa"))))))
;; #bdba81
;;-------------------windows--------------------------------------------

;;;Winner-mode
;;可以使用 Ctrl-c ← （就是向左的箭头键）组合键，退回你的上一个窗口设置。）
;;可以使用 Ctrl-c → （前进一个窗口设置。）
(when (fboundp 'winner-mode) 
  (winner-mode) 
  (windmove-default-keybindings)) 

;;;windmove-mode
(when (fboundp 'windmove-default-keybindings)
      (windmove-default-keybindings)
    (global-set-key (kbd "C-c j")  'windmove-left)
    (global-set-key (kbd "C-c l") 'windmove-right)
    (global-set-key (kbd "C-c i")    'windmove-up)
    (global-set-key (kbd "C-c k")  'windmove-down))

;;--------------------------Packages------------------------------------

;; Basic .emacs/init.el with a good set of defaults, to be used as template for usage
;; with OCaml and OPAM
;;
;; Author: Louis Gesbert <louis.gesbert@ocamlpro.com>
;; Released under CC0

;; Generic, recommended configuration options


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)


(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
;;(setq use-package-always-ensure t)

(use-package command-log-mode)
;; C-x C-e to evaluate ; M-x command-log-mode to check
;; Describe function by C-h f -> command-log-mode
;; M-x eval-buffer ; M-x global-command-log-mode ; M-x clm/toggle-command-log-buffer

;;@ivy
(use-package ivy
  :diminish
  :bind (("C-x s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;;@opam setup

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

;;;windresize
(require 'windresize)
 (global-set-key (kbd "C-c m") 'windresize)

;; config emacs for OCaml according to https://www.lri.fr/~filliatr/ens/compil/.emacs
;; ajouter le rÃ©pertoire emacs d'opam
(setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))


;; charger le mode merlin
(require 'merlin)

;; lancer merlin sur les fichiers OCaml
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)

;; activer auto-complete
;(setq merlin-use-auto-complete-mode 'easy)
(ac-config-default)
(setq merlin-ac-setup 'easy)

;(define-key ac-mode-map (kbd [backtab]) 'auto-complete)
(ac-set-trigger-key "TAB")
; auto start
(setq ac-auto-start t)

; finish completion by TAB
(define-key ac-completing-map "\t" 'ac-complete)
(define-key ac-completing-map "\r" nil)

; config auto-complete
(setq ac-use-menu-map t)
;; Default settings
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
(setq ac-use-quick-help t)
;; 20 lines
(setq ac-menu-height 20)


;; utiliser opam avec merlin
(setq merlin-command 'opam)

;; des raccourcis pratiques
(global-set-key [f5] 'compile)
(global-set-key [f6] 'recompile)
(global-set-key [f7] 'next-error)

;; pour avoir des raccourcis C-c C-v C-x pour le copier-coller (is already applied at the top)
;; (custom-set-variables '(cua-mode t nil (cua-base)))

; ocp-indent
(require 'ocp-indent)
;(setq ocp-indent-path
;     (concat
;      (replace-regexp-in-string "\n$" ""
;          (shell-command-to-string "opam config var bin")) "/ocp-indent"))
(setq ocp-indent-config "strict_with=always,match_clause=4,strict_else=never")

;; add ocamlformat, and change the kbd to C-<tab>
(require 'ocamlformat)
(add-hook 'tuareg-mode-hook (lambda ()
  (define-key tuareg-mode-map (kbd "C-<tab>") #'ocamlformat)
  (add-hook 'before-save-hook #'ocamlformat-before-save)))

; with use-package of ocamlformat
;;(use-package ocamlformat
  ;;:custom (ocamlformat-enable 'enable-outside-detected-project)
  ;;:hook (before-save . ocamlformat-before-save)
  ;;)


;;----------------Other packages-------------
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'init-markdown)

;;tiny

;;helm
(require 'helm)

;;----------------C/C++Programming-------------------

;; useful
(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd-12"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

;;(require 'setup-general)
;;(if (version< emacs-version "24.4")
;;    (require 'setup-ivy-counsel)
;;  (require 'setup-helm)
;;  (require 'setup-helm-gtags))
;; (require 'setup-ggtags)
;;(require 'setup-cedet)
;;(require 'setup-editing)

;; useful
;;headers
(require 'ac-c-headers)
(add-hook 'c-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-c-headers)
            (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))

(add-hook 'c++-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-c-headers)
            (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))

;;(require 'ac-clang)

;;(when (ac-clang-initialize)
;;  (add-hook 'c-mode-common-hook '(lambda ()
;;                                   (setq clang-server-cflags CFLAGS)
;;                                   (ac-clang-activate-after-modify))))

;; company

;;(require 'company
;;  :config
;;  (progn
;;    (add-hook 'after-init-hook 'global-company-mode)
;;    (global-set-key (kbd "M-/") 'company-complete-common-or-cycle)
;;    (setq company-idle-delay 0)))


;; yasnippet
;; (require 'yasnippet)
;; (yas-global-mode 1)

;;flycheck
;;(require 'flycheck
;;  :config
;;  (progn
;;    (global-flycheck-mode)))

;;useful
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)

;; useful
;;xcscope
(require 'xcscope)
(cscope-setup)

;;cc-mode

;;(add-to-list 'load-path "~/.emacs.d/lisp/cc-mode-5.35")
;;(require 'cc-mode)
;;(c-set-offset 'inline-open 0)
;;(c-set-offset 'friend '-)
;;(c-set-offset 'substatement-open 0)

;;(defun my-c-mode-common-hook()
;;  (setq tab-width 4 indent-tabs-mode nil)
;;  ;;; hungry-delete and auto-newline
;;  (c-toggle-auto-hungry-state 1)
;;  ;;按键定义
;;  (define-key c-mode-base-map [(control \`)] 'hs-toggle-hiding)
;;  (define-key c-mode-base-map [(return)] 'newline-and-indent)
;;  (define-key c-mode-base-map [(f7)] 'compile)
;;  (define-key c-mode-base-map [(meta \`)] 'c-indent-command)
;;;;  (define-key c-mode-base-map [(tab)] 'hippie-expand)
;;;;  (define-key c-mode-base-map [(tab)] 'my-indent-or-complete)
;;;;  (define-key c-mode-base-map [(meta ?/)] 'semantic-ia-complete-symbol-menu)
;;  (setq c-macro-shrink-window-flag t)
;;  (setq c-macro-preprocessor "cpp")
;;  (setq c-macro-cppflags " ")
;;  (setq c-macro-prompt-flag t)
;;  (setq hs-minor-mode t)
;;  (setq abbrev-mode t)
;;)

;;(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;;(defun my-c++-mode-hook()
;;  (setq tab-width 4 indent-tabs-mode nil)
;;  (c-set-style "stroustrup")
;;;;  (define-key c++-mode-map [f3] 'replace-regexp)
;;)
