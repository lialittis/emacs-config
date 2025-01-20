(setq user-full-name "Tianchi YU")
(setq user-mail-address "yu-tianchi@outlook.com")

(require 'package)
;; 设置 ELPA 源（官方源）
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
			             ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")))

;; 也可以使用国内源加速（如 TUNA、清华大学开源镜像）
;; (setq package-archives '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;                          ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
;;                          ("org"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

;; 刷新软件包列表（如果第一次运行）
;; (unless package-archive-contents
  ;; (package-refresh-contents))

;;;
;; 加载目录和子目录
;;(defun add-subdirs-to-load-path (dir)
;;  "Recursive add directories to `load-path'."
;;  (let ((default-directory (file-name-as-directory dir)))
;;    (add-to-list 'load-path dir)
;;    (normal-top-level-add-subdirs-to-load-path)))
(defun add-subdirs-to-load-path (search-dir)
  (interactive)
  (let* ((dir (file-name-as-directory search-dir)))
    (dolist (subdir
             ;; 过滤出不必要的目录，提升Emacs启动速度
             (cl-remove-if
              #'(lambda (subdir)
                  (or
                   ;; 不是目录的文件都移除
                   (not (file-directory-p (concat dir subdir)))
                   ;; 父目录、 语言相关和版本控制目录都移除
                   (member subdir '("." ".." 
                                    "dist" "node_modules" "__pycache__" 
                                    "RCS" "CVS" "rcs" "cvs" ".git" ".github")))) 
              (directory-files dir)))
      (let ((subdir-path (concat dir (file-name-as-directory subdir))))
        ;; 目录下有 .el .so .dll 文件的路径才添加到 `load-path' 中，提升Emacs启动速度
        (when (cl-some #'(lambda (subdir-file)
                           (and (file-regular-p (concat subdir-path subdir-file))
                                ;; .so .dll 文件指非Elisp语言编写的Emacs动态库
                                (member (file-name-extension subdir-file) '("el" "so" "dll"))))
                       (directory-files subdir-path))

          ;; 注意：`add-to-list' 函数的第三个参数必须为 t ，表示加到列表末尾
          ;; 这样Emacs会从父目录到子目录的顺序搜索Elisp插件，顺序反过来会导致Emacs无法正常启动
          (add-to-list 'load-path subdir-path t))

        ;; 继续递归搜索子目录
        (add-subdirs-to-load-path subdir-path)))))
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
   '(cuda-mode markdown-preview-mode ws-butler company-rtags company-irony-c-headers company-irony company-c-headers irony flycheck-pyflakes company auto-complete-c-headers auto-complete-clang xcscope term+ apt-sources-list windsize flymake-shell helm-ls-hg helm-ls-git helm-flyspell helm-flycheck helm-ctest helm flycheck window-numbering)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
