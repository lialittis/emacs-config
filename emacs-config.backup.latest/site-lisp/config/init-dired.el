(setq dired-dwim-target t) ;; copy

;; single buffer
(put 'dired-find-alternate-file 'disabled nil)

;; 主动加载 Dired Mode
;; (require 'dired)
;; (defined-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)

;; 延迟加载
(with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
    (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))))  ; was dired-up-directory)

;; sort
(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program nil)
(setq ls-lisp-verbosity nil)

(defun custom-dired-sort-dir-first ()
  "Dired sort hook to list directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and (featurep 'xemacs)
       (fboundp 'dired-insert-set-properties)
       (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))

(add-hook 'dired-after-readin-hook 'custom-dired-sort-dir-first)

(provide 'init-dired)

;;;; Reference

;; https://github.com/slegetank/.emacs.d/blob/master/init/init-dired.org
