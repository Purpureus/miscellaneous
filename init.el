(require 'cc-mode)

;; Window configuration
(add-to-list 'default-frame-alist '(maximized))
(desktop-save-mode 1)
(add-hook 'after-init-hook '_set-darkroot-theme)
(setq inhibit-startup-message t)
(setq read-buffer-completion-ignore-case t)

;; Default file
(setq default-file-path "E:\\")
(defun _startup ()
  (interactive)
  (find-file default-file-path)
  )

;; Importing minor modes
(add-to-list 'load-path "~/.emacs.d/modes")

(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'js-mode-hook 'emmet-mode)
(setq emmet-expand-jsx-className? t)

(defun _jsx ()
  (interactive)
  (js-jsx-mode)
  )

;; Scss mode
(require 'scss-mode)

;; Cursor & navigation
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
(setq truncate-lines 0)

					; Disable middle mouse btn
(global-unset-key [mouse-2])

;; Set buffer limit to a lot
(setq undo-limit 2000000)
(setq undo-strong-limit 4000000)

(setq dabbrev-case-replace t)
(setq dabbrev-case-fold-search t)
(setq dabbrev-upcase-means-case-search t)

;; --------------------------------
;; FORMATTING
;; --------------------------------

;; Formatting for all modes in CC Mode.
(defconst buebo-c-style
  '((c-tab-always-indent        . nil)
    (c-comment-only-line-offset . 4)
    (c-offsets-alist            . ((arglist-close . 0)
                                   (arglist-cont-nonempty . 6)
                                   (substatement-open . 0)
                                   (case-label        . 4)
                                   (block-open        . 4)
                                   kill  (knr-argdecl-intro . -)
                                   (brace-list-open   . 4)
                                   (statement-case-open . 0)
                                   (comment-intro . 0)
                                   (statement-block-intro . 4)
                                   (statement . 0)
                                   (statement-cont . 0)
                                   (topmost-intro-cont . 4)
                                   ))
    (c-echo-syntactic-information-p . t)
    )
  "Buebo CC style")

(defun my-c-mode-common-hook ()
  (c-add-style "PERSONAL" buebo-c-style t)
  (c-toggle-auto-newline -1)
  (c-toggle-comment-style -1)
  (setq tab-width 4
        indent-tabs-mode nil)
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; Formatting for web mode
(setq-local web-mode-markup-indent-offset 4)

(defun buebo-html-hook ()
  (setq sgml-basic-offset 4)
  (setq indent-tabs-mode nil)
  )

(add-hook 'html-mode-hook 'buebo-html-hook)
(add-hook 'sgml-mode-hook 'buebo-html-hook)
(add-hook 'sass-mode-hook 'buebo-html-hook)
(add-hook 'scss-mode-hook 'buebo-html-hook)

;; -----------------------------------
;; COMMANDS
;; -----------------------------------
(setq initial-buffer-choice "~/.emacs.d/init.el")

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

(defun vsp ()
  (interactive)
  (split-window-horizontally)
  )

(defun sp ()
  (interactive)
  (split-window-vertically))

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode)
  )

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode)
  )

(defun previous-blank-line ()
  (interactive)
  (if (eq (search-backward-regexp "^[ \t]*\n" nil t) nil)
      (progn
        (beginning-of-buffer)
        (beginning-of-line))
    )
  )

(defun next-blank-line ()
  (interactive)
  (forward-line)
  (if (eq (search-forward-regexp "^[ \t]*\n" nil t) nil)
      (progn
        (end-of-buffer)
        (end-of-visual-line))
    (forward-line -1)
    )
  )

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

(defun indent-and-newline ()
  (interactive)
  (indent-region (point) (mark))
  (newline))

(defun open-explorer-here ()
  (interactive)
  (shell-command "explorer ."))


;; --------------------------------
;; KEYMAPS
;; --------------------------------
(defvar buebo-mode-map (make-keymap) "buebo-mode-map keymap")
(defvar buebo-global-mode-map (make-keymap) "buebo-global-mode-keymap")

					; Misc keymaps
(define-key buebo-global-mode-map (kbd "C-<backspace>") 'backward-delete-word)
(define-key buebo-global-mode-map (kbd "M-g") 'goto-line)
(define-key buebo-global-mode-map (kbd "C-<tab>") (kbd "C-x o"))
(define-key buebo-global-mode-map (kbd "M-<f4>") 'kill-emacs)
(define-key buebo-mode-map (kbd "C-SPC") 'newline)
(define-key buebo-mode-map (kbd "C-z") 'undo)

					; Navigate using C-hjkl
(define-key buebo-mode-map (kbd "C-h") 'backward-char)
(define-key buebo-mode-map (kbd "C-j") 'next-line)
(define-key buebo-mode-map (kbd "C-k") 'previous-line)
(define-key buebo-mode-map (kbd "C-l") 'forward-char)

(define-key buebo-mode-map (kbd "C-n") 'backward-word)
(define-key buebo-mode-map (kbd "C-m") 'next-blank-line)
(define-key buebo-mode-map (kbd "C-,") 'previous-blank-line)
(define-key buebo-mode-map (kbd "C-.") 'forward-word)

(define-key buebo-mode-map (kbd "C-y") 'backward-delete-word)
(define-key buebo-mode-map (kbd "C-o") 'kill-word)
(define-key buebo-mode-map (kbd "C-u") 'delete-backward-char)
(define-key buebo-mode-map (kbd "C-i") 'delete-forward-char)

					; Mark and operate
(define-key buebo-mode-map (kbd "C-q") 'set-mark-command)
(define-key buebo-mode-map (kbd "C-w") 'clipboard-kill-ring-save)
(define-key buebo-mode-map (kbd "C-e") 'yank)
(define-key buebo-mode-map (kbd "C-r") 'clipboard-kill-region)

(define-key buebo-mode-map (kbd "C-f") 'delete-horizontal-space)

(define-key buebo-mode-map (kbd "C-t") 'move-line-up)
(define-key buebo-mode-map (kbd "C-g") 'move-line-down)
(define-key buebo-mode-map (kbd "C-b") 'delete-region)

					; Global keybindings (work on special modes such as dired and shell)
(dolist (m (list buebo-global-mode-map))

  (define-key m (kbd "C-'") 'dabbrev-expand)
  (define-key m (kbd "C-º") 'kill-buffer)
  (define-key m (kbd "C-+") 'execute-extended-command)

  (define-key m (kbd "M-<left>") 'switch-to-prev-buffer)
  (define-key m (kbd "M-<right>") 'switch-to-next-buffer)
  (define-key m (kbd "M-<up>") 'switch-to-buffer)

					; Navigation with tab
  (define-key m (kbd "<tab> l") 'end-of-line)
  (define-key m (kbd "<tab> h") 'beginning-of-line)
  (define-key m (kbd "<tab> k") 'beginning-of-buffer)
  (define-key m (kbd "<tab> j") 'end-of-buffer)

                                        ; Line operations
  (define-key m (kbd "<tab> y") 'kill-whole-line)
  (define-key m (kbd "<tab> u") 'comment-line)
  (define-key m (kbd "<tab> i") 'select-line-no-whitespace)

  (define-key m (kbd "<tab> q") 'region-fill-lines)
  (define-key m (kbd "<tab> w") 'select-function)

  (define-key m (kbd "<tab> a") 'delete-region)
  (define-key m (kbd "<tab> s") 'delete-rectangle)

  (define-key m (kbd "<tab> º") 'kill-buffer-and-window)
  (define-key m (kbd "<tab> 1") 'buffer-menu)
  (define-key m (kbd "<tab> 2") 'save-buffer)
  (define-key m (kbd "<tab> 9") 'eval-buffer)

  (define-key m (kbd "<tab> b") 'fill-lines-and-delete)
  (define-key m (kbd "<tab> <tab>") 'indent-region)
  )

;; --------------------------------
;; Minor mode for keymaps
;; --------------------------------
(defun turn-on-buebo-mode () (interactive) (buebo-mode 1) )
(define-minor-mode buebo-mode "Pinky-friendly minor mode" nil 'buebo-mode-map)
(define-minor-mode buebo-global-mode "Pinky-friendly GLOBAL minor mode" nil 'buebo-global-mode-map)

(add-hook 'sqml-mode-hook 'buebo-mode)
(add-hook 'html-mode-hook 'buebo-mode)
(add-hook 'css-mode-hook 'buebo-mode)
(add-hook 'haml-mode-hook 'buebo-mode)
(add-hook 'sass-mode-hook 'buebo-mode)
(add-hook 'js-mode-hook 'buebo-mode)
(add-hook 'c-mode-common-hook 'buebo-mode)
(add-hook 'cc-mode-hook 'buebo-mode)
(add-hook 'cpp-mode-hook 'buebo-mode)
(add-hook 'emacs-lisp-mode-hook 'buebo-mode)
(add-hook 'buffer-menu-mode-hook 'buebo-mode)
(add-hook 'help-mode-hook 'buebo-mode)
(add-hook 'text-mode-hook 'buebo-mode)
(add-hook 'web-mode-hook 'buebo-mode)
(add-hook 'js-jsx-mode-hook 'buebo-mode)

(add-hook 'sqml-mode-hook 'buebo-global-mode)
(add-hook 'html-mode-hook 'buebo-global-mode)
(add-hook 'css-mode-hook 'buebo-global-mode)
(add-hook 'haml-mode-hook 'buebo-global-mode)
(add-hook 'sass-mode-hook 'buebo-global-mode)
(add-hook 'js-mode-hook 'buebo-global-mode)
(add-hook 'c-mode-common-hook 'buebo-global-mode)
(add-hook 'cc-mode-hook 'buebo-global-mode)
(add-hook 'cpp-mode-hook 'buebo-global-mode)
(add-hook 'emacs-lisp-mode-hook 'buebo-global-mode)
(add-hook 'buffer-menu-mode-hook 'buebo-global-mode)
(add-hook 'help-mode-hook 'buebo-global-mode)
(add-hook 'text-mode-hook 'buebo-global-mode)
(add-hook 'web-mode-hook 'buebo-global-mode)
(add-hook 'js-jsx-mode-hook 'buebo-global-mode)

(add-hook 'shell-mode-hook 'buebo-global-mode)
(add-hook 'dired-mode-hook 'buebo-global-mode)

;; --------------------------------------
;; THEME
;; --------------------------------------
(defun _set-darkroot-theme ()
  (interactive)
  (set-background-color "#09261c")
  (set-foreground-color "#a0ebc0")

  (set-face-attribute 'default nil
                      :family "Liberation Mono"
                      :height 110
                      :weight 'normal
                      :width 'normal)

  (set-cursor-color "#0ff")
  (set-face-attribute 'region nil :background "#066" :foreground "#f4f4f4")
  (set-face-attribute 'font-lock-builtin-face nil :weight 'bold :foreground "#abc")
  (set-face-attribute 'font-lock-comment-face nil :weight 'bold :foreground "#fff")
  (set-face-attribute 'font-lock-constant-face nil :weight 'bold :foreground "#cab")
  (set-face-attribute 'font-lock-doc-face nil :foreground "gray50")
  (set-face-attribute 'font-lock-function-name-face nil :weight 'bold :foreground "#cba")
  (set-face-attribute 'font-lock-keyword-face nil :weight 'bold :foreground "#ccc")
  (set-face-attribute 'font-lock-string-face nil :foreground "#fff")
  (set-face-attribute 'font-lock-type-face nil :foreground "#eee")
  (set-face-attribute 'font-lock-variable-name-face nil :foreground "#acb")
  )

(defun _set-red-theme ()
  (interactive)
  (set-background-color "#332222")
  (set-foreground-color "#eee")

  (set-face-attribute 'default nil
                      :family "Liberation Mono"
                      :height 110
                      :weight 'normal
                      :width 'normal)

  (set-cursor-color "#f66")
  (set-face-attribute 'region nil :background "#844" :foreground "#f4f4f4")
  (set-face-attribute 'font-lock-builtin-face nil :weight 'bold :foreground "#db9")
  (set-face-attribute 'font-lock-comment-face nil :weight 'bold :foreground "#fff")
  (set-face-attribute 'font-lock-constant-face nil :weight 'bold :foreground "#f44")
  (set-face-attribute 'font-lock-doc-face nil :foreground "#aad")
  (set-face-attribute 'font-lock-function-name-face nil :weight 'bold :foreground "#f88")
  (set-face-attribute 'font-lock-keyword-face nil :weight 'bold :foreground "#c88")
  (set-face-attribute 'font-lock-string-face nil :foreground "#fff")
  (set-face-attribute 'font-lock-type-face nil :foreground "#fda")
  (set-face-attribute 'font-lock-variable-name-face nil :foreground "#eee")
  )

(defun _set-light-theme ()
  (interactive)
  (set-background-color "#ede9de")
  (set-foreground-color "#000")

  (set-face-attribute 'default nil
                      :family "Liberation Mono"
                      :height 110
                      :weight 'normal
                      :width 'normal)

  (set-cursor-color "#000")
  (set-face-attribute 'region nil :background "#852" :foreground "#f4f4f4")
  (set-face-attribute 'font-lock-builtin-face nil :weight 'bold :foreground "#420")
  (set-face-attribute 'font-lock-comment-face nil :weight 'bold :foreground "#000")
  (set-face-attribute 'font-lock-constant-face nil :weight 'bold :foreground "#876")
  (set-face-attribute 'font-lock-doc-face nil :foreground "#880")
  (set-face-attribute 'font-lock-function-name-face nil :weight 'bold :foreground "#630")
  (set-face-attribute 'font-lock-keyword-face nil :weight 'bold :foreground "#000")
  (set-face-attribute 'font-lock-string-face nil :foreground "#630")
  (set-face-attribute 'font-lock-type-face nil :foreground "#630")
  (set-face-attribute 'font-lock-variable-name-face nil :foreground "#111")
  )

(defun _set-brown-theme ()
  (interactive)

  (setq default-foreground "#d3af86")
  (setq comment-foreground "#fff")
  (setq string-foreground "#aaa")

  (set-background-color "#221a0f")
  (set-foreground-color default-foreground)

  (set-face-attribute 'default nil
                      :family "Liberation Mono"
                      :height 110
                      :weight 'normal
                      :width 'normal)

  (set-cursor-color "#fff")

  (set-face-attribute 'region nil :weight 'normal
                      :background "#ccc"
                      :foreground "#000")

  (set-face-attribute 'font-lock-builtin-face nil :weight 'normal
                      :foreground default-foreground)

  (set-face-attribute 'font-lock-comment-face nil :weight 'normal
                      :foreground comment-foreground)

  (set-face-attribute 'font-lock-constant-face nil :weight 'normal
                      :foreground default-foreground)

  (set-face-attribute 'font-lock-doc-face nil :weight 'normal
                      :foreground default-foreground)

  (set-face-attribute 'font-lock-function-name-face nil :weight 'normal
                      :foreground default-foreground)

  (set-face-attribute 'font-lock-keyword-face nil :weight 'normal
                      :foreground "#ddd")

  (set-face-attribute 'font-lock-string-face nil :weight 'normal
                      :foreground string-foreground)

  (set-face-attribute 'font-lock-type-face nil :weight 'normal
                      :foreground default-foreground)

  (set-face-attribute 'font-lock-variable-name-face nil :weight 'normal
                      :foreground default-foreground)
  )

(dolist (m (list c-mode-base-map emacs-lisp-mode-map))
  (define-key m (kbd "<tab> t 1") '_set-light-theme)
  (define-key m (kbd "<tab> t 2") '_set-darkroot-theme)
  (define-key m (kbd "<tab> t 3") '_set-red-theme)
  (define-key m (kbd "<tab> t 4") '_set-brown-theme)
  )

;; DEFAULT THEME
(_set-darkroot-theme)
