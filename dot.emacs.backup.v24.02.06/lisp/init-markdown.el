;;-------------------------
;; 配置markdown语法支持
;;-------------------------
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(autoload 'markdown-mode "markdown-mode.el"
"Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

(provide 'init-markdown)
