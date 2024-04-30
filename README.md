# Make Emacs Better!

本库用来保存我的emacs配置，最新的emacs配置是基于Emacs29以及Emacs大佬Lazycat的配置和教程，将进行周期性的整理。

## treesitter


## eaf


## lsp-bridge


## C/C++ 开发

### 头文件查看

1. find-file【内置】

增加、修改查找路径，并增加快捷键操作：
```lisp
(setq cc-search-directories '("./" "/usr/include/c++/11" "/usr/include" "/usr/local/include/*"))
(global-set-key (kbd "C-x C-o") 'ff-find-other-file)
```
如果对具体路径下的代码做查找路径补充，例如：
```lisp
((c++-mode
  (eval . (add-to-list 'cc-search-directories "~/MyProject/include"))
)
```

### 找到定义或引用
1. xref

2. lsp-bridge

3. flycheck

针对不同项目预设不同的flycheck路径（在.dir-locals.el）：
```lisp
;; .dir-locals.el
((c++-mode
  (eval . (add-to-list 'flycheck-clang-include-path "/usr/include"))
  (eval . (add-to-list 'flycheck-clang-include-path "/usr/local/include"))
  (eval . (add-to-list 'flycheck-clang-include-path "/usr/include/c++/8"))
  (eval . (add-to-list 'flycheck-clang-include-path "/usr/include/clang"))
  (eval . (add-to-list 'flycheck-clang-include-path "~/MyProject/include"))
  )
```
