;;; init-window.el --- Init window

;; Filename: init-window.el
;; Description: Init window
;; Author: Andy Stewart <andy@freedom>
;; Maintainer: Andy Stewart <andy@freedom>
;; Copyright (C) 2013, Andy Stewart, all rights reserved.
;; Created: 2013-12-30 14:54:31
;; Version: 0.1
;; Last-Updated: 2013-12-30 14:54:31
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-window.el
;; Keywords:
;; Compatibility: GNU Emacs 24.3.50.1
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
;; Init window
;;

;;; Installation:
;;
;; Put init-window.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-window)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-window RET
;;

;;; Change log:
;;
;; 2013/12/30
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

;;(one-key-create-menu
;; "WINDOW-NAVIGATION"
;; '(
;;   (("j" . "Downward") . windmove-down)
;;   (("k" . "Upward") . windmove-up)
;;   (("h" . "Leftward") . windmove-left)
;;   (("l" . "Rightward") . windmove-right)
;;   (("s" . "Move Down") . buf-move-down)
;;   (("d" . "Move Up") . buf-move-up)
;;   (("a" . "Move Left") . buf-move-left)
;;   (("f" . "Move Right") . buf-move-right)
;;   (("u" . "Enlarge Down") . (lambda () (interactive) (windresize-up-inwards '-1)))
;;   (("i" . "Enlarge Up") . (lambda () (interactive) (windresize-down-inwards '-1)))
;;   (("y" . "Enlarge Left") . (lambda () (interactive) (windresize-right-inwards '-1)))
;;   (("o" . "Enlarge Right") . (lambda () (interactive) (windresize-left-inwards '-1)))
;;   (("m" . "Shrink Down") . (lambda () (interactive) (windresize-up-inwards '1)))
;;   (("," . "Shrink Up") . (lambda () (interactive) (windresize-down-inwards '1)))
;;   (("n" . "Shrink Left") . (lambda () (interactive) (windresize-right-inwards '1)))
;;   (("." . "Shrink Right") . (lambda () (interactive) (windresize-left-inwards '1)))
;;   (("x" . "Outward Window") . outward-window)
;;   (("c" . "Inward Window") . inward-window)
;;   ((";" . "Kill Buffer") . kill-this-buffer)
;;   ((":" . "Kill Other Windows") . delete-other-windows)
;;   (("'" . "Kill Buffer And Window") . delete-current-buffer-and-window)
;;   (("e" . "List Registers") . list-registers)
;;   (("r" . "Remember Register") . frame-configuration-to-register)
;;   (("t" . "Jump Register") . jump-to-register)
;;   (("g" . "Split Horizontally") . split-window-horizontally)
;;   (("v" . "Split Vertically") . split-window-vertically)
;;   )
;;t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window numbering @tianchi
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package window-numbering installed from package list
;; Allows switching between buffers using meta-(# key)
;;  (when (display-graphic-p)
;;(window-numbering-mode t)
;;    )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up helm @tianchi
;; 实现文件路径自动buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load helm and set M-x to helm, buffer to helm, and find files to herm
(require 'helm)
(require 'helm-ls-git)
(require 'helm-ctest)
;; Use C-c h for helm instead of C-x c
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c C-t") 'helm-ctest)
(setq
 helm-split-window-in-side-p           t
                                        ; open helm buffer inside current window,
                                        ; not occupy whole other window
 helm-move-to-line-cycle-in-source     t
                                        ; move to end or beginning of source when
                                        ; reaching top or bottom of source.
 helm-ff-search-library-in-sexp        t
                                        ; search for library in `require' and `declare-function' sexp.
 helm-scroll-amount                    8
                                        ; scroll 8 lines other window using M-<next>/M-<prior>
 helm-ff-file-name-history-use-recentf t
 ;; Allow fuzzy matches in helm semantic
 helm-semantic-fuzzy-match t
 helm-imenu-fuzzy-match    t)
;; Have helm automaticaly resize the window
(helm-autoresize-mode 1)
(setq rtags-use-helm t)
(require 'helm-flycheck) ;; Not necessary if using ELPA package
(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))


(provide 'init-window)

;;; init-window.el ends here
