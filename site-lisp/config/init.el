;; 加速配置。
(require 'init-accelerate)

;; TO UPDATE
;; pressing C-x C-o to find file, path is not completed

;; 字体设置
;; (require 'init-font)

(let (
      ;; 加载的时候临时增大`gc-cons-threshold'以加速启动速度。
      (gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.6)
      ;; 清空避免加载远程文件的时候分析文件。
      (file-name-handler-alist nil))

  ;; 让窗口启动更平滑
  (setq frame-inhibit-implied-resize t)

  (setq-default inhibit-redisplay t
                inhibit-message t)
  (add-hook 'window-setup-hook
            (lambda ()
              (setq-default inhibit-redisplay nil
                            inhibit-message nil)
              (redisplay)))

  ;; 定义一些启动目录，方便下次迁移修改
  (defvar my-emacs-root-dir (file-truename "~/MyEmacs/Site-Lisp"))
  (defvar my-emacs-config-dir (concat my-emacs-root-dir "/config"))
  (defvar my-emacs-extension-dir (concat my-emacs-root-dir "/extensions"))

  (with-temp-message ""              ;抹掉插件启动的输出
    ;;(require 'benchmark-init-modes)
    ;;(require 'benchmark-init)
    ;;(benchmark-init/activate)
    (require 'init-idle)
    (require 'init-packages)
    (require 'init-fullscreen)
    (require 'init-generic)
    (require 'lialittis-theme)
    ;; ;; (lazycat-theme-load-with-sunrise)
    (lialittis-theme-load-dark)
    ;; ;; (lazycat-theme-load-light)
    ;; (when (featurep 'cocoa)
    ;;   (require 'cache-path-from-shell))
    (require 'lazy-load)
    (require 'one-key)
    ;; (require 'fingertip)
    (require 'display-line-numbers)
    ;; (require 'basic-toolkit)
    ;; (require 'redo)
    ;; (require 'mind-wave)
    (require 'init-mode)

    (require 'init-flycheck-mode)
    (require 'init-highlight-parentheses)
    (require 'init-awesome-tray)
    (require 'init-line-number)
    (require 'init-auto-save)
    ;; (require 'init-fingertip)
    (require 'init-one-key)
    ;; (require 'init-key)
    ;; (require 'init-vi-navigate)
    ;; (require 'init-isearch-mb)
    ;; (require 'init-performance)
    ;; (require 'init-rime)
    ;; (require 'init-mind-wave)
    ;; (require 'init-key-echo)
    (require 'init-kbd-shortcuts)
    (require 'init-lsp-bridge)
    (require 'init-indent)

    ;; 可以延后加载的
    (run-with-idle-timer
     1 nil
     #'(lambda ()
        (message "Start to init other packages")
        
        
        (require 'init-markdown-mode)
        (require 'init-window)
        (require 'init-blink-search)
        (require 'init-thing-edit)
        (require 'init-eaf)
        (require 'goto-line-preview)
        (require 'init-c)
        (require 'init-treesit)

        ;;  (require 'pretty-lambdada)
        ;;  (require 'browse-kill-ring)
        ;;  (require 'elf-mode)

        ;;  (require 'init-eldoc)
        (require 'init-yasnippet)
        ;;  (require 'init-cursor-chg)
        ;;  (require 'init-winpoint)
        ;;  (require 'init-info)
        ;; (require 'init-golang)
        ;;  (require 'init-org)

        ;;  (require 'init-olivetti)
        ;;  (require 'init-holo-layer)

        ;;  (require 'init-popweb)

        ;;  (require 'trekker)
        ;;  (trekker-enable)
        ;;  ;; Restore session at last.
        (require 'init-session)
        ;;  (emacs-session-restore)

        (require 'init-sort-tab)
        ;; (require 'init-llvm)
        (message "DONE")
         ))))

(provide 'init)
