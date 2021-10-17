(require 'cc-mode)

                                        ; Window configuration
(add-to-list 'default-frame-alist '(maximized))
(desktop-save-mode 1)
;; (add-hook 'after-init-hook '_set-darkroot-theme)
(setq inhibit-startup-message t)
(setq read-buffer-completion-ignore-case t)

;; Default file
(setq default-file-path "E:\\")
(defun _startup ()
  (interactive)
  (find-file default-file-path)
  )

                                        ; Importing minor modes
(add-to-list 'load-path "~/.emacs.d/modes")
(load "~/.emacs.d/themes")

(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'js-mode-hook 'emmet-mode)
(setq emmet-expand-jsx-className? t)

(add-hook 'emacs-startup-hook '_set-brown-theme)

(add-to-list 'auto-mode-alist '("\\.glsl\\'" . c-mode))

                                        ; Cursor & navigation
(blink-cursor-mode 0)
(show-paren-mode 1)
(setq show-paren-delay 0)
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(setq split-width-treshold nil)
(setq scroll-margin 10)
(setq scroll-conservatively 10)
(setq tab-width 4)
(electric-pair-mode)
;; (setq truncate-lines t)
(setq visual-line-mode nil)
(delete-selection-mode)

;; Disable middle mouse btn
(global-unset-key [mouse-2])

;; Set buffer limit to a lot
(setq undo-limit 999999)
(setq undo-strong-limit 4000000)

(setq dabbrev-case-replace t)
(setq dabbrev-case-fold-search t)
(setq dabbrev-upcase-means-case-search t)

;; -------------------- ;;
;; ---- FORMATTING ---- ;;
;; -------------------- ;;

;; Formatting for all modes in CC Mode.
(defconst purpureus-c-style
  '(
    (c-tab-always-indent . nil)
    (setq-default c-basic-offset 4)
    (c-comment-only-line-offset . 4)
    (c-toggle-comment-style -1)
    (c-offsets-alist
     . (
        (arglist-close . 0)
        (arglist-cont-nonempty . c-lineup-arglist)
        (substatement-open . 0)
        (case-label        . 4)
        (block-open        . 4)
        (brace-list-open   . 4)
        (statement-case-open . 0)
        (comment-intro . 0)
        (statement-block-intro . 4)
        (statement . 0)
        (statement-cont . 0)
        (topmost-intro-cont . 0)
        )
     )
    (c-echo-syntactic-information-p . t)
    )
  "Purpureus CC style")

(defun my-c-mode-common-hook ()
  (c-add-style "Purpureus style" purpureus-c-style t)
  (c-toggle-auto-newline -1)
  (c-toggle-comment-style -1)
  (setq tab-width 4
        indent-tabs-mode nil)
  )

(add-hook 'cc-mode-hook 'my-c-mode-common-hook)
(add-hook 'c-mode-hook 'my-c-mode-common-hook)

;; ------------------------ ;;
;; ---- WEB MODE STUFF ---- ;;
;; ------------------------ ;;
;; (setq-local web-mode-markup-indent-offset 4)

;; (defun purpureus-html-hook ()
;;   (setq sgml-basic-offset 4)
;;   (setq indent-tabs-mode nil)
;;   )

;; (add-hook 'html-mode-hook 'purpureus-html-hook)
;; (add-hook 'sgml-mode-hook 'purpureus-html-hook)
;; (add-hook 'sass-mode-hook 'purpureus-html-hook)
;; (add-hook 'scss-mode-hook 'purpureus-html-hook)

;; -----------------------------------
;; COMMANDS
;; -----------------------------------
(setq initial-buffer-choice "~/.emacs.d/init.el")

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))
(add-hook 'emacs-startup-hook 'remove-dos-eol)

(defun reform-document ()
  (interactive)
  (setq cursor-location (point))
  (untabify 0 (point-max))
  (indent-region 0 (point-max))
  (replace-regexp "[ ]+$" "" nil 0 (point-max))
  (goto-char cursor-location)
  )

(defun _config ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun _themes ()
  (interactive)
  (find-file "~/.emacs.d/themes.el"))

(defun vsp ()
  (interactive)
  (split-window-horizontally)
  )

(defun sp ()
  (interactive)
  (split-window-vertically))

(defun select-line ()
  (interactive)
  (beginning-of-line)
  (set-mark-command nil)
  (end-of-line)
  )

(defun select-function ()
  (interactive)
  (beginning-of-defun)
  (set-mark-command nil)
  (end-of-defun))

(defun region-fill-lines ()
  (interactive)
  (setq mark-pos (mark))
  (setq point-pos (point))
  (cond
   ((eq mark-pos (max mark-pos point-pos))
    (beginning-of-line)
    (exchange-point-and-mark)
    (end-of-line)
    (exchange-point-and-mark))

   ((eq point-pos (max mark-pos point-pos))
    (end-of-line)
    (exchange-point-and-mark)
    (beginning-of-line)
    (exchange-point-and-mark))
   )
  )

(defun select-line-no-whitespace ()
  (interactive)
  (beginning-of-line-text)
  (set-mark-command nil)
  (end-of-line)
  )

(defun fill-lines-and-delete ()
  (interactive)
  (region-fill-lines)
  (delete-region (point) (mark)))

(defun backward-delete-word (arg)
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(defun copy-if-nonempty ()
  (interactive)
  (if mark-active
      (progn
        (if (/= (region-beginning) (region-end))
            (clipboard-kill-ring-save (point) (mark))
          ))))

(defun cut-if-nonempty ()
  (interactive)
  (if mark-active
      (progn
        (if (/= (region-beginning) (region-end))
            (clipboard-kill-region (point) (mark))
          ))))

(defun indent-and-newline ()
  (interactive)
  (indent-region (point) (mark))
  (newline))

(defun open-explorer-here ()
  (interactive)
  (shell-command "explorer ."))

;; ---- MOVE TEXT ---- ;;
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
        (transpose-lines arg))
      (forward-line -1)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(defun delete-word ()
  (interactive)
  (kill-word 1)
  (pop kill-ring)
  )

(defun surround-with-paren ()
  (interactive)
  (insert-pair -1 ?\( ?\))
  )

(defun surround-with-braces ()
  (interactive)
  (insert-pair -1 ?\{ ?\})
  )

(defun surround-with-brackets ()
  (interactive)
  (insert-pair -1 ?\[ ?\])
  )

(defun surround-with-quotes ()
  (interactive)
  (insert-pair -1 ?\' ?\')
  )

(defun surround-with-double-quotes ()
  (interactive)
  (insert-pair -1 ?\" ?\")
  )

(defun highlight-region ()
  (interactive)
  (unhighlight-regexp t)
  (if mark-active
	  (progn
		(highlight-phrase (buffer-substring (region-beginning) (region-end)))
		(deactivate-mark)
		)
	)
  )

(defun write-paren ()
  (interactive)
  (insert "()")
  (backward-char)
  )

(defun write-braces ()
  (interactive)
  (insert "{}")
  (backward-char)
  )

(defun write-brackets ()
  (interactive)
  (insert "[]")
  (backward-char)
  )

(defun end-with-semicolon ()
  (interactive)
  (end-of-line)
  (insert ";")
  )

;; ----------------- ;;
;; ---- KEYMAPS ---- ;;
;; ----------------- ;;
(global-set-key (kbd "M-<f4>") 'kill-emacs)

(global-set-key (kbd "C-!") 'write-paren)
(global-set-key (kbd "C-\"") 'write-braces)
(global-set-key (kbd "C-·") 'write-brackets)
(global-set-key (kbd "C-$") 'end-with-semicolon)

;; CTRL+KEY
(global-set-key (kbd "C-+") 'execute-extended-command)

(global-set-key (kbd "C-6") 'beginning-of-line)
(global-set-key (kbd "C-7") 'move-text-down)
(global-set-key (kbd "C-8") 'move-text-up)
(global-set-key (kbd "C-9") 'end-of-line)

(global-set-key (kbd "C-y") 'backward-delete-word)
(global-set-key (kbd "C-u") 'delete-backward-char)
(global-set-key (kbd "C-i") 'delete-char)
(global-set-key (kbd "C-o") 'delete-word)

(global-set-key (kbd "C-h") 'backward-char)
(global-set-key (kbd "C-j") 'next-line)
(global-set-key (kbd "C-k") 'previous-line)
(global-set-key (kbd "C-l") 'forward-char)

(global-set-key (kbd "C-n") 'backward-word)
(global-set-key (kbd "C-m") 'forward-paragraph)
(global-set-key (kbd "C-,") 'backward-paragraph)
(global-set-key (kbd "C-.") 'forward-word)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-x") 'cut-if-nonempty)
(global-set-key (kbd "C-c") 'copy-if-nonempty)
(global-set-key (kbd "C-v") 'yank)

(global-set-key (kbd "C-1") 'select-line-no-whitespace)
(global-set-key (kbd "C-2") 'select-function)
(global-set-key (kbd "C-3") 'region-fill-lines)
(global-set-key (kbd "C-4") 'kill-whole-line)
(global-set-key (kbd "C-5") 'comment-line)

(global-set-key (kbd "C-q") 'set-mark-command)
(global-set-key (kbd "C-w") 'rectangle-mark-mode)
(global-set-key (kbd "C-e") 'indent-region)
(global-set-key (kbd "C-r") 'highlight-region)
(global-set-key (kbd "C-t") 'delete-horizontal-space)

(global-set-key (kbd "C-'") 'dabbrev-expand)
(global-set-key (kbd "C-SPC") 'newline)
(global-set-key (kbd "C-<tab>") 'other-window)

;; CTRL+SHIFT+KEY
;; ? and ¿ = Shift + keys that go after the number row
(global-set-key (kbd "C-?") 'previous-buffer)
(global-set-key (kbd "C-¿") 'next-buffer)
(global-set-key (kbd "C-S-C") 'cut-if-nonempty)

;; Ctrl+ç KEY
(global-set-key (kbd "C-ç ESC") 'kill-this-buffer)
(global-set-key (kbd "C-ç C-ç ESC") 'delete-window)
(global-set-key (kbd "C-ç s") 'save-buffer)

(global-set-key (kbd "C-ç x") 'open-explorer-here)
(global-set-key (kbd "C-ç e") 'eval-buffer)
(global-set-key (kbd "C-ç f") 'find-file)
(global-set-key (kbd "C-ç b") 'switch-to-buffer)

(global-set-key (kbd "C-ç r") 'read-only-mode)
(global-set-key (kbd "C-ç t") '_set-next-theme)
(global-set-key (kbd "C-ç g") 'goto-line)
(global-set-key (kbd "C-ç c") 'copy-if-nonempty)

(global-set-key (kbd "C-ç 7") 'end-of-buffer)
(global-set-key (kbd "C-ç 8") 'beginning-of-buffer)

(global-set-key (kbd "C-ç C-1") 'surround-with-paren)
(global-set-key (kbd "C-ç C-2") 'surround-with-braces)
(global-set-key (kbd "C-ç C-3") 'surround-with-brackets)
(global-set-key (kbd "C-ç C-4") 'surround-with-double-quotes)
(global-set-key (kbd "C-ç C-5") 'surround-with-quotes)

;; specific mode rebindings
(define-key c-mode-map (kbd "C-i") 'delete-char)
(define-key c-mode-map (kbd "C-c") 'copy-if-nonempty)

;; --------------- ;;
;; ---- THEME ---- ;;
;; --------------- ;;
(setq current-theme -1)

(defun _set-theme (arg)
  (interactive)
  (cond
   ((= 0 current-theme) (_set-brown-theme))
   ((= 1 current-theme) (_set-darkroot-theme))
   ((= 2 current-theme) (_set-gray-theme))
   ((= 3 current-theme) (_set-red-theme))
   ((= 4 current-theme) (_set-light-theme))
   )
  )

(defun _set-next-theme ()
  (interactive)

  (set 'current-theme (1+ current-theme))
  (cond
   ((> current-theme 4)
    (set 'current-theme 0))
   )

  (_set-theme current-theme)
  )
