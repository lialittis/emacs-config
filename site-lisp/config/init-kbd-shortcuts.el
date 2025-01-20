
;; ;; fingertip-mod
;; ;;
;; (define-key fingertip-mode-map (kbd "(") 'fingertip-open-round)
;; (define-key fingertip-mode-map (kbd "[") 'fingertip-open-bracket)
;; (define-key fingertip-mode-map (kbd "{") 'fingertip-open-curly)
;; (define-key fingertip-mode-map (kbd ")") 'fingertip-close-round)
;; (define-key fingertip-mode-map (kbd "]") 'fingertip-close-bracket)
;; (define-key fingertip-mode-map (kbd "}") 'fingertip-close-curly)
;; (define-key fingertip-mode-map (kbd "=") 'fingertip-equal)

;; (define-key fingertip-mode-map (kbd "（") 'fingertip-open-chinese-round)
;; (define-key fingertip-mode-map (kbd "「") 'fingertip-open-chinese-bracket)
;; (define-key fingertip-mode-map (kbd "【") 'fingertip-open-chinese-curly)
;; (define-key fingertip-mode-map (kbd "）") 'fingertip-close-chinese-round)
;; (define-key fingertip-mode-map (kbd "」") 'fingertip-close-chinese-bracket)
;; (define-key fingertip-mode-map (kbd "】") 'fingertip-close-chinese-curly)

;; (define-key fingertip-mode-map (kbd "%") 'fingertip-match-paren)
;; (define-key fingertip-mode-map (kbd "\"") 'fingertip-double-quote)
;; (define-key fingertip-mode-map (kbd "'") 'fingertip-single-quote)

;; (define-key fingertip-mode-map (kbd "SPC") 'fingertip-space)
;; (define-key fingertip-mode-map (kbd "RET") 'fingertip-newline)

;; (define-key fingertip-mode-map (kbd "M-o") 'fingertip-backward-delete)
;; (define-key fingertip-mode-map (kbd "C-d") 'fingertip-forward-delete)
;; (define-key fingertip-mode-map (kbd "C-k") 'fingertip-kill)

;; (define-key fingertip-mode-map (kbd "M-\"") 'fingertip-wrap-double-quote)
;; (define-key fingertip-mode-map (kbd "M-'") 'fingertip-wrap-single-quote)
;; (define-key fingertip-mode-map (kbd "M-[") 'fingertip-wrap-bracket)
;; (define-key fingertip-mode-map (kbd "M-{") 'fingertip-wrap-curly)
;; (define-key fingertip-mode-map (kbd "M-(") 'fingertip-wrap-round)
;; (define-key fingertip-mode-map (kbd "M-)") 'fingertip-unwrap)

;; (define-key fingertip-mode-map (kbd "M-p") 'fingertip-jump-right)
;; (define-key fingertip-mode-map (kbd "M-n") 'fingertip-jump-left)
;; (define-key fingertip-mode-map (kbd "M-:") 'fingertip-jump-out-pair-and-newline)

;; (define-key fingertip-mode-map (kbd "C-j") 'fingertip-jump-up)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Keyboard Shortcuts  @tianchi
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set help to C-?
(global-set-key (kbd "C-?") 'help-command)
;; Set mark paragraph to M-?
(global-set-key (kbd "M-?") 'mark-paragraph)
;; Set backspace to C-h
(global-set-key (kbd "C-h") 'delete-backward-char)
;; Set backspace word to M-h
(global-set-key (kbd "M-h") 'backward-kill-word)
;; Use meta+tab word completion
(global-set-key (kbd "M-TAB") 'dabbrev-expand)
;; Easy undo key
(global-set-key (kbd "C-/") 'undo)
;; Comment or uncomment the region
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
;; Indent after a newline, if required by syntax of language
(global-set-key (kbd "C-m") 'newline-and-indent)
;; Load the compile ocmmand
(global-set-key (kbd "C-c C-c") 'compile)
;; Undo, basically C-x u
(global-set-key (kbd "C-/") 'undo)

;;; Cumtmize key
;; Smart copy, if no region active, it simply copy the current whole line
(defadvice kill-line (before check-position activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode
                                c-mode c++-mode objc-mode js-mode
                                latex-mode plain-tex-mode))
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))
 
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end))
                 (message "Copied line")
                 (list (line-beginning-position)
                       (line-beginning-position 2)))))
 
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))
 
;; Copy line from point to the end, exclude the line break
(defun qiang-copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (point)
                  (line-end-position))
                  ;; (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))
 
(global-set-key (kbd "M-k") 'qiang-copy-line)

;;; Additional Functions
(global-set-key (kbd "C-x C-o") 'ff-find-other-file)

;;; move wind
(global-set-key (kbd "C-c j") 'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c i") 'windmove-up)
(global-set-key (kbd "C-c k") 'windmove-down)



(provide 'init-kbd-shortcuts)
