;; (let ((default-directory  "~/.config/emacs/modules"))
;;   (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path "~/.config/emacs/lisp")
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes")


(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

(straight-use-package 'use-package)
(eval-when-compile (require 'use-package))

(use-package autothemer
  :straight t
  :ensure t
  :init (load-theme 'rose-pine-moon t))

;; (straight-use-package
;;   '(catppuccin :type git :host github :repo "catppuccin/emacs"))
;; (load-theme 'catppuccin t)
;; (setq catppuccin-flavor 'macchiato) ;; 'frappe, 'latte, 'macchiato, or 'mocha
;; (catppuccin-reload)

(require 'nano-layout)

;; https://sqrtminusone.xyz/configs/emacs/
(when (display-graphic-p)
  (set-frame-font "PragmataPro Mono Liga 15" nil t)
  (set-face-attribute 'variable-pitch nil :family "ETBookOT" :height 1.0))

;; change font size, interactively
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(use-package evil-anzu
  :straight t
  :ensure t
  :config (global-anzu-mode +1))

(use-package doom-modeline
  :straight t
  :ensure t
  :config
  (doom-modeline-mode 1))

(use-package hide-mode-line
  :straight t
  :ensure t
  :config
  (add-hook 'completion-list-mode-hook #'hide-mode-line-mode))

(use-package general
  :straight t
  :demand t
  :config
  (general-evil-setup)

  (general-create-definer lc/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "S-SPC")

  (general-create-definer lc/local-leader-keys
    :states '(normal visual)
    :keymaps 'override
    :prefix ","
    :global-prefix "SPC m")

  (general-nmap
    :states 'normal
    "gD" '(xref-find-references :wk "references"))

  ;; (lc/leader-keys
  ;;   "`" '((lambda () (interactive) (switch-to-buffer (other-buffer (current-buffer) 1))) :which-key "prev buffer")
  ;;   "<escape>" 'keyboard-escape-quit)
  ;;   "SPC" '(execute-extended-command :which-key "execute command"))
)

(use-package evil
  :straight t
  :demand t
  :general
  (lc/leader-keys
    "wv" 'evil-window-vsplit
    "ws" 'evil-window-split
    "wd" 'evil-window-delete
    "wh" 'evil-window-left
    "wj" 'evil-window-down
    "wk" 'evil-window-up
    "wl" 'evil-window-right
    "bd" 'kill-this-buffer)
  :init
  ;; (setq-default evil-shift-width 2)
  (setq evil-undo-system 'undo-redo)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-search-module 'evil-search)  ;; enables gn
  (setq evil-want-keybinding nil)
  ;; move to window when splitting
  (setq-local evil-scroll-count 0)
  ;; (setq evil-want-integration t)
  ;; (setq evil-want-C-i-jump t)
  ;; (setq evil-respect-visual-line-mode t)
  ;; (setq evil-auto-indent nil)
  :config
  (evil-mode 1)
  ;; (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;; (define-key evil-motion-state-map "_" 'evil-end-of-line)
  ;; (define-key evil-motion-state-map "0" 'evil-beginning-of-line)
  ;; (evil-set-initial-state 'messages-buffer-mode 'normal)
  ;; (evil-set-initial-state 'dashboard-mode 'normal)
  ;; ;; don't move cursor after ==
  ;; (defun lc/evil-dont-move-cursor (orig-fn &rest args)
  ;;   (save-excursion (apply orig-fn args)))
  ;; (advice-add 'evil-indent :around #'lc/evil-dont-move-cursor)
)

(use-package evil-goggles
  :straight t
  :ensure t
  :config
  (evil-goggles-mode)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

(use-package evil-nerd-commenter
  :straight t
  :ensure t
  :general
  (lc/leader-keys
    "c" 'evilnc-comment-operator
    "C" 'evilnc-copy-and-comment-operator)
  (general-nmap
    "<tab>" 'evil-next-buffer
    "<backtab>" 'evil-next-buffer))

;; (straight-use-package 'evil-collection)
;; (use-package evil-collection
;;   :after evil
;;   :demand
;;   :init
;;   (setq evil-collection-magit-use-z-for-folds nil)
;;   :config
;;   (evil-collection-init))

;; (use-package doom-themes
;;   :straight t
;;   :ensure t
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   ;; (load-theme 'doom-one t)
;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; or for treemacs users
;;   ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
;;   ;; (doom-themes-treemacs-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

