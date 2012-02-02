;; insert spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; show column numbers
(column-number-mode t)

;; show matching parens
(show-paren-mode 1)

;; show time
(display-time)

;; resize buffer hotkeys
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; hotkeys
(global-set-key (kbd "M-z") 'undo)
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "M-t") 'dirtree)
(global-set-key (kbd "C-t") 'sr-speedbar-toggle)

(global-set-key (kbd "M-j") 'next-line)
(global-set-key (kbd "M-k") 'previous-line)
(global-set-key (kbd "M-h") 'backward-char)
(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-J") 'forward-paragraph)
(global-set-key (kbd "M-K") 'backward-paragraph)
(global-set-key (kbd "M-H") 'backward-word)
(global-set-key (kbd "M-L") 'forward-word)

;; function collapsing
(outline-minor-mode)
(global-set-key (kbd "M-6") 'hide-sublevels)
(global-set-key (kbd "M-7") 'show-all)
(global-set-key (kbd "M-8") 'hide-subtree)
(global-set-key (kbd "M-9") 'show-subtree)

;; tuareg mode
(add-to-list 'load-path "~/.emacs.d/tuareg-2.0.4")
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
(setq auto-mode-alist 
      (append '(("\\.ml[ily]?$" . tuareg-mode)
		("\\.topml$" . tuareg-mode))
	      auto-mode-alist))

;; emacs goodies --tabbar
(add-to-list 'load-path "~/.emacs.d/emacs-goodies-el")
(require 'tabbar)
(tabbar-mode)
(global-set-key (kbd "<C-left>") 'tabbar-backward-tab)
(global-set-key (kbd "<C-right>") 'tabbar-forward-tab)
(global-set-key (kbd "<C-up>") 'tabbar-backward-group)
(global-set-key (kbd "<C-down>") 'tabbar-forward-group)

;; color theme
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
(add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized")
(require 'color-theme-solarized)
(color-theme-solarized-dark)

;; line numbers
(add-to-list 'load-path "~/.emacs.d")
(require 'linum)
(global-linum-mode 1)

;; sr-speedbar
(require 'sr-speedbar)
(setq-default sr-speedbar-right-side nil)


;; enable mouse clicking in emacs
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))
(setq mouse-sel-mode t)

;; mouse control and mouse scroll
(require 'mwheel)

;; directory trees
(autoload 'dirtree "dirtree" "Add directory to tree view" t)

;; highlight 80 characters
(require 'highlight-80+)
(add-hook 'find-file-hook 'highlight-80+-mode)

;; delete extra whitespace
(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")
  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))
  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion
      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))
      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )
      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let
  ;; put the point in the lowest line and return
  (next-line arg))