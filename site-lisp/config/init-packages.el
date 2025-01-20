;; You might already have this line
(package-initialize)
;; list the packages you want
(defvar package-list)
(setq package-list '(flycheck helm helm-core helm-ctest helm-flycheck 
                     helm-flyspell helm-ls-git helm-ls-hg 
                     window-numbering))

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))
;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


(setq package-selected-packages
      '(cuda-mode
        markdown-preview-mode
        ws-butler
        company-rtags
        company-irony-c-headers
        company-irony
        company-c-headers
        irony
        flycheck-pyflakes
        company
        auto-complete-c-headers
        auto-complete-clang
        xcscope
        term+
        apt-sources-list
        windsize
        flymake-shell
        helm-ls-hg
        helm-ls-git
        helm-flyspell
        helm-flycheck
        helm-ctest
        helm
        flycheck
        window-numbering))

;; 安装缺失的包
(package-install-selected-packages)

;; 另一部分的扩展添加，在LialittisDepend.el
;;(require 'LialittisDepend)

(provide 'init-packages)
