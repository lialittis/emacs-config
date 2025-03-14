# Make Emacs Better!

本库用来保存我的emacs配置，最新的emacs配置是基于Emacs29,并且深度依赖Emacs大佬Lazycat的[配置](https://github.com/manateelazycat/lazycat-emacs)和教程。
因此本库将对代码进行周期性的调整和归置，并逐渐对库的原始路径进行更新，代码的来源均来自于Lazycat及各类开源库，本人系新进emacser，才疏学浅，仅在其基础上进行了微调和补充。


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

| Key Binding                | Function                         |
|----------------------------|----------------------------------|
| `M-,`                 | `xref-go-back`      |
| `C-M-,`                 | `xref-go-forward`       |
| `M-.`                 | `xref-find-definitions`       |

2. lsp-bridge

#### Pass initialization options to `clangd`.

There are two approaches to achieve this.

The first option is configuring the options directly in `lsp-bridge`, but it might need more customed fuctions to do this.

[TODO]

The second option is using a compilation database, by generate a `compile_commands.json` file in the project directory.
To do this, you only need add this into your CMakeLists.txt file

```
# Add this line, to enable compile command export
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
```

or alternatively, you could pass this option along as arguments, e.g.

```
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON .
```



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
