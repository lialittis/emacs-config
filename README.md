# Make Emacs Better!

本库用来保存我的emacs配置，最新的emacs配置是基于Emacs29以及Emacs大佬Lazycat的配置和教程，将进行周期性的整理。

首先，以基础的功能性为展示类目。

## 初始化布局

- session 的保存和恢复函数配置 init-session.el
- 标签栏：[sort-tab](https://github.com/manateelazycat/sort-tab) 会根据文件使用的频率自动对标签进行排序， 用的越多的 Buffer 越靠左边。下面为自定义快捷键：

| Key Binding                | Function                         |
|----------------------------|----------------------------------|
| `C-x M-1`                 | `sort-tab-select-first-tab`      |
| `C-x M-p`                 | `sort-tab-select-prev-tab`       |
| `C-x M-n`                 | `sort-tab-select-next-tab`       |
| `C-x M-Q`                 | `sort-tab-close-all-tabs`        |
| `C-x M-q`                 | `sort-tab-close-current-tab`     |

- 页脚栏：最下面是 [awesome-tray](https://github.com/manateelazycat/awesome-tray), awesome-tray 的优点是直接隐藏 mode-line, 把必须的状态信息（比如位置、 mode、 日期、 时间、 父目录、 Git 信息等）放到 minibuffer 最右边。

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
