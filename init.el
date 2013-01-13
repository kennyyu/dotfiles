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
(global-set-key (kbd "C-b") 'ecb-activate)
(global-set-key (kbd "M-b") 'ecb-deactivate)
(global-set-key (kbd "C-w") 'delete-trailing-whitespace)

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

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
(add-to-list 'load-path "~/.emacs.d/tuareg-2.0.5")
(add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . tuareg-mode))
(autoload 'tuareg-mode "tuareg" "Major mode for editing OCaml code" t)
(autoload 'tuareg-run-ocaml "tuareg" "Run an inferior OCaml process." t)
(autoload 'ocamldebug "ocamldebug" "Run the OCaml debugger" t)

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

;; emacs goodies --tabbar
(add-to-list 'load-path "~/.emacs.d/emacs-goodies-el")
(require 'tabbar)
(tabbar-mode)
(global-set-key (kbd "<C-left>") 'tabbar-backward-tab)
(global-set-key (kbd "<C-right>") 'tabbar-forward-tab)
(global-set-key (kbd "<C-up>") 'tabbar-backward-group)
(global-set-key (kbd "<C-down>") 'tabbar-forward-group)

;; directory trees
(autoload 'dirtree "dirtree" "Add directory to tree view" t)

;; highlight 80 characters
(require 'highlight-80+)
(add-hook 'find-file-hook 'highlight-80+-mode)

;; delete extra whitespace
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

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

;; CEDET
(load-file "~/.emacs.d/cedet-1.0pre6/common/cedet.el")
(global-ede-mode 1)                      ; Enable the Project management system
(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion
(global-srecode-minor-mode 1)            ; Enable template insertion menu

;; ECB
(add-to-list 'load-path "~/.emacs.d/ecb-2.40")
(require 'ecb)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-layout-name "left-directories-right-methods")
 '(ecb-layout-window-sizes (quote (("left-directories-right-methods" (0.18232044198895028 . 0.9836065573770492) (0.22099447513812154 . 0.9836065573770492)))))
 '(ecb-options-version "2.40")
 '(ecb-other-window-behavior (quote all))
 '(ecb-show-sources-in-directories-buffer (quote always))
 '(gud-gdb-command-name "gdb --annotate=1")
 '(large-file-warning-threshold nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
