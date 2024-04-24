;;; init-qt.el --- Setup for qt mode

;; Filename: init-qt.el
;; Description: Setup for qt mode
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2017, Andy Stewart, all rights reserved.
;; Created: 2017-02-06 21:29:16
;; Version: 0.1
;; Last-Updated: 2017-02-06 21:29:16
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-qt.el
;; Keywords:
;; Compatibility: GNU Emacs 25.0.50.1
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Setup for qt mode
;;

;;; Installation:
;;
;; Put init-qt.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-qt)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-qt RET
;;

;;; Change log:
;;
;; 2017/02/06
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require

;;; Code:
(dolist (hook (list
               'c-mode-hook
               'c++-mode-hook
               'c-mode-common-hook
               ))
  (add-hook hook #'(lambda ()
                     (require 'cc-mode)
                     (require 'modern-cpp-font-lock)

                     (defun c-mode-style-setup ()
                       (interactive)
                       ;; cpp font lock.
                       (modern-c++-font-lock-global-mode t)

                       ;; base-style
                       (c-set-style "stroustrup")

                       ;; qt keywords and stuff ...
                       ;; set up indenting correctly for new qt kewords
                       (setq c-protection-key (concat "\\<\\(public\\|public slot\\|protected"
                                                      "\\|protected slot\\|private\\|private slot"
                                                      "\\)\\>")
                             c-C++-access-key (concat "\\<\\(signals\\|public\\|protected\\|private"
                                                      "\\|public slots\\|protected slots\\|private slots"
                                                      "\\)\\>[ \t]*:"))
                       (progn
                         ;; modify the colour of slots to match public, private, etc ...
                         (font-lock-add-keywords 'c++-mode
                                                 '(("\\<\\(slots\\|signals\\)\\>" . font-lock-type-face)))
                         ;; make new font for rest of qt keywords
                         (make-face 'qt-keywords-face)
                         (set-face-foreground 'qt-keywords-face "DeepSkyBlue1")
                         ;; qt keywords
                         (font-lock-add-keywords 'c++-mode
                                                 '(("\\<Q_OBJECT\\>" . 'qt-keywords-face)))
                         (font-lock-add-keywords 'c++-mode
                                                 '(("\\<SIGNAL\\|SLOT\\>" . 'qt-keywords-face)))
                         (font-lock-add-keywords 'c++-mode
                                                 '(("\\<Q[A-Z][A-Za-z]\\>" . 'qt-keywords-face)))
                         ))
                     (c-mode-style-setup)
                     
                     )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up code completion with company and irony @tianchi
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'company)
;; (require 'company-rtags)
;; ;; (global-company-mode)

;; ;; Enable semantics mode for auto-completion
;; ;; (require 'cc-mode)
;; (require 'semantic)
;; (global-semanticdb-minor-mode 1)
;; (global-semantic-idle-scheduler-mode 1)
;; (semantic-mode 1)

;; Setup irony-mode to load in c-modes
;; (require 'irony)
;; (require 'company-irony-c-headers)
;; (require 'cl)
;; (add-hook 'c++-mode-hook 'irony-mode)
;; (add-hook 'c-mode-hook 'irony-mode)
;; (add-hook 'objc-mode-hook 'irony-mode)

;; irony-mode hook that is called when irony is triggered
(defun my-irony-mode-hook ()
  "Custom irony mode hook to remap keys."
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

;; (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; company-irony setup, c-header completions
;; (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;; Remove company-semantic because it has higher precedance than company-clang
;; Using RTags completion is also faster than semantic, it seems. Semantic
;; also provides a bunch of technically irrelevant completions sometimes.
;; All in all, RTags just seems to do a better job.
;; (setq company-backends (delete 'company-semantic company-backends))


;; Enable company-irony and several other useful auto-completion modes
;; We don't use rtags since we've found that for large projects this can cause
;; async timeouts. company-semantic (after company-clang!) works quite well
;; but some knowledge some knowledge of when best to trigger is still necessary.
;; (eval-after-load 'company
;;   '(add-to-list
;;     'company-backends '(company-irony-c-headers
;;                         company-irony company-yasnippet
;;                         company-clang company-rtags)
;;     )
;;   )

(defun my-disable-semantic ()
  "Disable the company-semantic backend."
  (interactive)
  (setq company-backends (delete '(company-irony-c-headers
                                   company-irony company-yasnippet
                                   company-clang company-rtags
                                   company-semantic) company-backends))
  (add-to-list
   'company-backends '(company-irony-c-headers
                       company-irony company-yasnippet
                       company-clang company-rtags))
  )
(defun my-enable-semantic ()
  "Enable the company-semantic backend."
  (interactive)
  (setq company-backends (delete '(company-irony-c-headers
                                   company-irony company-yasnippet
                                   company-clang) company-backends))
  (add-to-list
   'company-backends '(company-irony-c-headers
                       company-irony company-yasnippet company-clang))
  )

;; ;; Zero delay when pressing tab
;; (setq company-idle-delay 0)
;; (define-key c-mode-map [(tab)] 'company-complete)
;; (define-key c++-mode-map [(tab)] 'company-complete)
;; ;; Delay when idle because I want to be able to think without
;; ;; completions immediately being called and slowing me down.
;; (setq company-idle-delay 0.2)

;; ;; Prohibit semantic from searching through system headers. We want
;; ;; company-clang to do that for us.
;; (setq-mode-local c-mode semanticdb-find-default-throttle
;;                  '(local project unloaded recursive))
;; (setq-mode-local c++-mode semanticdb-find-default-throttle
;;                  '(local project unloaded recursive))

;; (semantic-remove-system-include "/usr/include/" 'c++-mode)
;; (semantic-remove-system-include "/usr/local/include/" 'c++-mode)
;; (add-hook 'semantic-init-hooks
;;           'semantic-reset-system-include)

;; rtags Seems to be really slow sometimes so I disable using
;; it with irony mode
;; (require 'flycheck-rtags)
;; (defun my-flycheck-rtags-setup ()
;;   (flycheck-select-checker 'rtags)
;;   ;; RTags creates more accurate overlays.
;;   (setq-local flycheck-highlighting-mode nil)
;;   (setq-local flycheck-check-syntax-automatically nil))
;; ;; c-mode-common-hook is also called by c++-mode
;; (add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)

;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; Add flycheck to helm
(require 'helm-flycheck) ;; Not necessary if using ELPA package
(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'init-c)

;;; init-qt.el ends here
