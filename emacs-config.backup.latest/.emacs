(setq user-full-name "Tianchi YU")
(setq user-mail-address "yu-tianchi@outlook.com")
;;;
;; 加载目录和子目录
(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))
(add-subdirs-to-load-path "/home/tianchi/MyEmacs/Site-Lisp/")


(setq inhibit-startup-message nil)
(setq inhibit-startup-buffer-menu t)

;; 启动时是否连接网络。
;; 如果是， Emacs 会执行一些自动启动一些联网程序
;; 否则不启动
;;(defvar startup-connect-network t
;;  "Whether connect network when startup Emacs.")

(require 'init)

;; 加载个人设置
;;(require 'LialittisFont)                  ;字体
;;(require 'LialittisDepend)                ;依赖
;;(require 'LazyCatRedefine)              ;重定义
;;(require 'LazyCatKeystoke)              ;按键
;;(require 'LazyCatCustomize)             ;自定义
;;(require 'LazyCatSetup)                 ;设置
;;(require 'LazyCatTheme)                 ;主题
;;(require 'LazyCatStartup)               ;启动

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ws-butler company-rtags company-irony-c-headers company-irony company-c-headers irony flycheck-pyflakes company auto-complete-c-headers auto-complete-clang xcscope term+ apt-sources-list windsize flymake-shell helm-ls-hg helm-ls-git helm-flyspell helm-flycheck helm-ctest helm flycheck window-numbering)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
