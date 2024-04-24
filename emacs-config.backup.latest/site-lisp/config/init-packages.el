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

;; 另一部分的扩展添加，在LialittisDepend.el
;;(require 'LialittisDepend)

(provide 'init-packages)
