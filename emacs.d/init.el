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

;; Hotkeys for special functionality
(global-set-key (kbd "M-z") 'undo)               ;; Also C-/
(global-set-key (kbd "C-o") 'other-window)       ;; Normally C-x o
(global-set-key (kbd "C-t") 'sr-speedbar-toggle) ;; directory tree sidebar
(global-set-key (kbd "C-b") 'ecb-activate)       ;; directory & code outline bars
(global-set-key (kbd "M-b") 'ecb-deactivate)

;; Hotkeys VIM-like Movement
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

;; tuareg mode for ocaml
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
(load-file "~/.emacs.d/linum.el")
(require 'linum)
(global-linum-mode 1)

;; sr-speedbar for showing directory tree
(load-file "~/.emacs.d/dirtree.el")
(load-file "~/.emacs.d/tree-mode.el")
(load-file "~/.emacs.d/sr-speedbar.el")
(require 'sr-speedbar)
(setq-default sr-speedbar-right-side nil)

;; enable mouse clicking in emacs
(load-file "~/.emacs.d/mwheel.el")
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))
(setq mouse-sel-mode t)

;; mouse control and mouse scroll
(require 'mwheel)

;; emacs goodies and hotkeys for switching tabs
(load-file "~/.emacs.d/windata.el")
(add-to-list 'load-path "~/.emacs.d/emacs-goodies-el")
(require 'tabbar)
(tabbar-mode)
(global-set-key (kbd "<C-left>") 'tabbar-backward-tab)
(global-set-key (kbd "<C-right>") 'tabbar-forward-tab)
(global-set-key (kbd "<C-up>") 'tabbar-backward-group)
(global-set-key (kbd "<C-down>") 'tabbar-forward-group)

;; highlight 80 characters
(load-file "~/.emacs.d/highlight-80+.el")
(require 'highlight-80+)
(add-hook 'find-file-hook 'highlight-80+-mode)

;; delete extra whitespace on saving/writing files
(add-hook 'write-file-hooks 'delete-trailing-whitespace)
(add-hook 'before-saving-hooks 'delete-trailing-whitespace)

;; CEDET - necessary for ECB
(load-file "~/.emacs.d/cedet-1.0pre6/common/cedet.el")
(global-ede-mode 1)                      ; Enable the Project management system
(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion
(global-srecode-minor-mode 1)            ; Enable template insertion menu

;; ECB
(add-to-list 'load-path "~/.emacs.d/ecb-2.40")
(require 'ecb)
(defvar start-dir (getenv "PWD"))
(defvar start-dir-name (car (last (split-string start-dir "/"))))
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-layout-name "left-directories-right-methods")
 '(ecb-layout-window-sizes (quote (("left-directories-right-methods" (0.18232044198895028 . 0.9836065573770492) (0.22099447513812154 . 0.9836065573770492)))))
 '(ecb-options-version "2.40")
 '(ecb-other-window-behavior (quote all))
 '(ecb-source-path (list (list start-dir start-dir-name)))
 '(ecb-show-sources-in-directories-buffer (quote always))
 '(gud-gdb-command-name "gdb --annotate=1")
 '(large-file-warning-threshold nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
