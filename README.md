# Emacs Configuration

## Structure

The configuration of Emacs is acutally a Software Project. Three ways to configure it:

1. Main file is ~./emacs (a Emacs lisp file) or
2. ~/.emacs.d/init.el, or
3. $XDG_CONFIG_HOME/emacs/init.el (Emacs 28+)

Normally the second option is used.

### (load "subfile.el")

### (provide) and (require)

This is similar with "#ifndef ... #define ... #endif".

```
;; ~/.emacs.d/init.el
(add-to-list 'load-path "~/.emacs.d/my")
(require 'subfile)

;; ~/.emacs.d/my/subfile.el
(message "subfile.el loaded.")
(provide 'subfile)

```
## Add package

### Mannually

```
cd ~/.emacs.d
git clone https://github.com/joaotavora/sly.git
```

```
;; ~/.emacs.d/init.el
(add-to-list 'load-path "~/.emacs.d/sly")
(require 'sly-autoloads)

;; configure the package
(setq inferior-lisp-program "/opt/sbcl/bin/sbcl")
```

### package.el, load-path

### use-package


### custom.el


## Command (only in this configuration)

### Base

- C-s : Incremental search forward(isearch-forward)

- C-r : Incremental search backword(isearch-backward)

- ESC ESC ESC : quit buffer

- ESC : quit command ;; by global setting

- C-c : copy

- C-x : cut

- C-v : paste

### Core

**M-x:** internal commands

- M-x eval-buffer

- M-x load-file

**C-h f:** declare functions

### Sup

1. line

- C-n : next line ; next line in auto-complete (a.k.a ac) map

- C-p : last line ; last line in ac map

* ocaml compiler

- C-c C-s : utop

- C-c C-e : ocaml REPL for one function

- C-<tab> : ocamlformat [problem of how to use]

- <tab> : ocp-indent auto indent OCaml code ; ac trigger key

- f3 : next-match

- shift-f3 : prev-match

- f5 : compile

- f6 : recompile

- f7 : next error

- C-n C-a : create .mli file based on the current .ml file


2. Screen Separation

- C-x 3 : separate vertically  
- C-x 2 : separate horizontally
- C-x 1 : maximize current window
- C-x 0 : shut down current window


- C-c <- ： reset/back to the last setting
- C-c -> ： go forward to the next setting

3. shell

- M-x shell : open shell

## Packages installed

### melpa

### tuareg

### utop

### merlin

### ocamlformat

### auto-complete (by package melpa)

### about markdown

在.emacs.d/lisp目录中新建文件init-markdown.el文件。依赖安装包markdown-mode。代码如下：

;;-------------------------
;; 配置markdown语法支持
;;-------------------------
(require-package 'markdown-mode) ;; 依赖markdown-mode包
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

(provide 'init-markdown)
完成上述代码后，执行命令M-x load-file重新加载init.el。即可发现，目前Emacs已经支持Markdown
