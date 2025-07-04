;; package --- summary

;;; Code:

;;; Commentary:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; management
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("elpa"         . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
	("nongnu"       . "https://elpa.nongnu.org/nongnu/"))
      package-archive-priorities
      '(("melpa-stable" . 10)
        ("elpa"         . 5)
        ("melpa"        . 1)
	("nongnu"       . 0)))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; if we need to call debbuger on specific call
;;(debug-on-entry 'package-initialize)

;; if we need to refresh melpa pkg list
;;(package-refresh-contents)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Look and feel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
(use-package cyberpunk-theme
  :ensure
  :init)
(load-theme 'cyberpunk t)

;; Menu & scrollbar
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 1)
;; (advice-add 'menu-bar-open
;;             :around
;;             (lambda (orig-fun &rest args)
;;               (menu-bar-mode 1)
;;               (apply orig-fun args)
;;               (menu-bar-mode -1)))

;; no more splash scren
(setq-default inhibit-splash-screen t)

;; always have focused window bigger
(use-package golden-ratio
  :ensure t
  :hook (after-init . golden-ratio-mode)
  :custom
  (golden-ratio-exclude-modes '(occur-mode)))

;; dont have to type yes and no anymore, just y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; nice icons and tree like browser
(use-package all-the-icons
  :ensure t
  :defer
  :if (display-graphic-p))
(use-package all-the-icons-completion
  :ensure t
  :defer
  :hook (marginalia-mode . #'all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))
(use-package neotree
  :ensure t
  :bind ("<f5>" . neotree-toggle)
  :hook (emacs-startup . neotree)
  :custom
  (neo-theme 'icons)
  (neo-smart-open t)
  (neo-autorefresh t)
  (neo-window-width 35)
  (neo-toggle-window-keep-p t)
  (neo-show-hidden-files t)
  ;; takes too long to update on first try
  ;; (neo-vc-integration '(face char))
  (neo-display-action '(gopar/neo-display-fn))
  :init
  (defun gopar/neo-display-fn (buffer _alist)
    (let ((window-pos (if (eq neo-window-position 'left) 'left 'right)))
      (display-buffer-in-side-window buffer `((side . ,window-pos)
                                              (inhibit-same-window . t)
                                              (dedicated . t)
                                              (window-parameters
                                               (no-delete-other-windows . t)
                                               (no-other-window . t)))))))

;; use tab
(setq tab-bar-close-button-show nil
      tab-bar-separator "|"
      tab-bar-format '(tab-bar-format-tabs-groups
                       tab-bar-separator
                       tab-bar-format-align-right
                       tab-bar-format-global))
(defun my/sync-tab-bar-to-theme ()
  "Synchronize tab-bar faces with the current theme."
  (interactive)
  (let ((default-bg (face-background 'default))
         (default-fg (face-foreground 'default))
         (inactive-fg (face-foreground 'mode-line-inactive)))
    (custom-set-faces
      `(tab-bar ((t (:inherit default :background ,default-bg :foreground ,default-fg))))
      `(tab-bar-tab ((t (:inherit default :background ,default-fg :foreground ,default-bg))))
      `(tab-bar-tab-inactive ((t (:inherit default :background ,default-bg :foreground ,inactive-fg)))))))
(my/sync-tab-bar-to-theme)
(tab-bar-mode 1)
(global-set-key (kbd "<f3>") 'tab-previous)
(global-set-key (kbd "<f4>") 'tab-next)

;; quickly select window by number
;; (use-package winum
;;   :ensure t
;;   :config)
;; (setq winum-keymap
;;       (let ((map (make-sparse-keymap)))
;; 	(define-key map (kbd "M-\N{LATIN SMALL LETTER A WITH GRAVE}") (lambda () (interactive) (winum-select-window-0-or-10)(golden-ratio)))
;; 	(define-key map (kbd "M-\N{ampersand}") (lambda () (interactive) (winum-select-window-1)(golden-ratio)))
;; 	(define-key map (kbd "M-\N{LATIN SMALL LETTER E WITH ACUTE}") (lambda () (interactive) (winum-select-window-2)(golden-ratio)))
;; 	(define-key map (kbd "M-\N{Quotation mark}") (lambda () (interactive) (winum-select-window-3)(golden-ratio)))
;; 	(define-key map (kbd "M-\N{apostrophe}") (lambda () (interactive) (winum-select-window-4)(golden-ratio)))
;; 	(define-key map (kbd "M-\N{left parenthesis}") (lambda () (interactive) (winum-select-window-5)(golden-ratio)))
;; 	(define-key map (kbd "M-\N{hyphen-minus}") (lambda () (interactive) (winum-select-window-6)(golden-ratio)))
;; 	(define-key map (kbd "M-\N{LATIN SMALL LETTER E WITH GRAVE}") (lambda () (interactive) (winum-select-window-7)(golden-ratio)))
;; 	(define-key map (kbd "M-\N{LOW LINE}") (lambda () (interactive) (winum-select-window-8)(golden-ratio)))
;; 	(define-key map (kbd "M-\N{LATIN SMALL LETTER C CEDILLA}") (lambda () (interactive) (winum-select-window-9)(golden-ratio)))
;; 	map))
;; (require 'winum)
;; (winum-mode)


;; highlight block of code
(use-package indent-guide
  :ensure t
  :init)
(indent-guide-global-mode t)

;; Display line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; highlight current line - tried a lot of conf, seems i dont like it
;;(global-hl-line-mode t)
;;(set-face-background hl-line-face "color-69") works, need to find good color
;;(set-face-background hl-line-face "red")
;;(set-face-foreground hl-line-face "red")
;;(set-face-underline hl-line-face t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let emacs works everywhere the same way ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; redefine key in order to use emacs in putty. See doc here : https://www.emacswiki.org/emacs/PuTTY#toc8
;; PuTTY hack - terminal needs to be in SCO mode and connection>data>terminal>xterm-256color
(if (eq system-uses-terminfo t)
    (progn
      (define-key key-translation-map [\e] [\M])
      (define-key input-decode-map "\e[H" [home])
      (define-key input-decode-map "\e[F" [end])
      (define-key input-decode-map "\e[D" [S-left])
      (define-key input-decode-map "\e[C" [S-right])
      (define-key input-decode-map "\e[A" [S-up])
      (define-key input-decode-map "\e[B" [S-down])
      (define-key input-decode-map "\e[I" [prior])
      (define-key input-decode-map "\e[G" [next])
      (define-key input-decode-map "\e[M" [f1])
      (define-key input-decode-map "\e[Y" [S-f1])
      (define-key input-decode-map "\e[k" [C-f1])
      (define-key input-decode-map "\e\e[M" [M-f1])
      (define-key input-decode-map "\e[N" [f2])
      (define-key input-decode-map "\e[Z" [S-f2])
      (define-key input-decode-map "\e[l" [C-f2])
      (define-key input-decode-map "\e\e[N" [M-f2])
      (define-key input-decode-map "\e[O" [f3])
      (define-key input-decode-map "\e[a" [S-f3])
      (define-key input-decode-map "\e[m" [C-f3])
      (define-key input-decode-map "\e\e[O" [M-f3])
      (define-key input-decode-map "\e[P" [f4])
      (define-key input-decode-map "\e[b" [S-f4])
      (define-key input-decode-map "\e[n" [C-f4])
      (define-key input-decode-map "\e\e[P" [M-f4])
      (define-key input-decode-map "\e[Q" [f5])
      (define-key input-decode-map "\e[c" [S-f5])
      (define-key input-decode-map "\e[o" [C-f5])
      (define-key input-decode-map "\e\e[Q" [M-f5])
      (define-key input-decode-map "\e[R" [f6])
      (define-key input-decode-map "\e[d" [S-f6])
      (define-key input-decode-map "\e[p" [C-f6])
      (define-key input-decode-map "\e\e[R" [M-f6])
      (define-key input-decode-map "\e[S" [f7])
      (define-key input-decode-map "\e[e" [S-f7])
      (define-key input-decode-map "\e[q" [C-f7])
      (define-key input-decode-map "\e\e[S" [M-f7])
      (define-key input-decode-map "\e[T" [f8])
      (define-key input-decode-map "\e[f" [S-f8])
      (define-key input-decode-map "\e[r" [C-f8])
      (define-key input-decode-map "\e\e[T" [M-f8])
      (define-key input-decode-map "\e[U" [f9])
      (define-key input-decode-map "\e[g" [S-f9])
      (define-key input-decode-map "\e[s" [C-f9])
      (define-key input-decode-map "\e\e[U" [M-f9])
      (define-key input-decode-map "\e[V" [f10])
      (define-key input-decode-map "\e[h" [S-f10])
      (define-key input-decode-map "\e[_" [C-f10])
      (define-key input-decode-map "\e\e[V" [M-f10])
      (define-key input-decode-map "\e[W" [f11])
      (define-key input-decode-map "\e[i" [S-f11])
      (define-key input-decode-map "\e[u" [C-f11])
      (define-key input-decode-map "\e\e[W" [M-f11])
      (define-key input-decode-map "\e[X" [f12])
      (define-key input-decode-map "\e[j" [S-f12])
      (define-key input-decode-map "\e[v" [C-f12])
      (define-key input-decode-map "\e\e[X" [M-f12])))

;; Enable transient mark mode
(transient-mark-mode 1)

;; xterm
(use-package xterm-color
  :ensure t)
(setq compilation-environment '("TERM=xterm-256color"))

;; ILU mouse
(xterm-mouse-mode t)

;; UTF-8 all the things!
(define-coding-system-alias 'UTF-8 'utf-8)
(set-charset-priority 'unicode)
(setq locale-coding-system   'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom file
(setq custom-file "~/.emacs.custom")
(load custom-file)

;; manage emacs directories
(let ((backup-dir "~/.emacs.d/backups")
      (auto-saves-dir "~/.emacs.d/auto-saves/")
      (temporary-file-directory "~/.emacs.d/tmp/"))
  (dolist (dir (list backup-dir auto-saves-dir temporary-file-directory))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))
(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control t      ; Use version numbers on backups,
      kept-new-versions 5    ; keep some new versions
      kept-old-versions 2)   ; and some old ones, too

;; remember where cursor was when closing the file
(save-place-mode 1)

;; refresh file if modified on disk
(global-auto-revert-mode 1)

;; Delete trailing whitespace before saving buffers
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit (use git inside emacs)
;; https://www.youtube.com/watch?v=qPfJoeQCIvA
(use-package magit
  :ensure t
  :commands magit-get-current-branch
  :defer
  :bind ("C-x g" . magit)
  :hook (magit-mode . magit-wip-mode)
  :custom
  (magit-diff-refine-hunk 'all)
  (magit-process-finish-apply-ansi-colors t)
  (magit-format-file-function #'magit-format-file-all-the-icons)
  :init
  (defun magit/undo-last-commit (number-of-commits)
    "Undoes the latest commit or commits without loosing changes"
    (interactive "P")
    (let ((num (if (numberp number-of-commits)
                   number-of-commits
                 1)))
      (magit-reset-soft (format "HEAD^%d" num)))))
(use-package git-commit
  :ensure nil
  :after magit
  :hook (git-commit-setup . gopar/auto-insert-jira-ticket-in-commit-msg)
  :custom
  (git-commit-summary-max-length 80)
  :init)
(use-package git-gutter
  :ensure t
  :hook (after-init . global-git-gutter-mode))
(global-set-key (kbd "<f6>") 'magit)

;; forge <= seems we can't install forge on debian sid like this, just apt-get install elpa-magit-forge instead
;; (use-package forge
;;   :ensure t
;;   :init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; complete code
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)

  (setq yas-snippet-dir "~/.emacs.d/snippets"))

;; vertical interactive completion
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; pop up completion
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match t)        ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match 'insert) ;; Configure handling of exact matches
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 3)
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  )
(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p))

;; use corfu in terminal too
(use-package corfu-terminal
  :ensure t)
(unless (display-graphic-p)
  (corfu-terminal-mode +1))

;; check in bufferS for completion
(use-package dabbrev
  :defer t
  :custom
  (dabbrev-upcase-means-case-search t)
  (dabbrev-check-all-buffers t)
  (dabbrev-check-other-buffers t)
  (dabbrev-friend-buffer-function 'dabbrev--same-major-mode-p)
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; Autocompletion
(use-package cape
  :ensure t
  :bind ("TAB" . cape-dabbrev)
  :custom
  (cape-dict-case-replace nil)
  (cape-dabbrev-buffer-function 'cape-same-mode-buffers)
  :init
;;  (defun gopar/cape-dict-only-in-comments ()
;;    (cape-wrap-inside-comment 'cape-dict))
;;  (defun gopar/cape-dict-only-in-strings ()
;;    (cape-wrap-inside-string 'cape-dict))
;;  (defun gopar/cape-yasnippet-keyword-dabbrev ()
;;    (cape-wrap-super #'yasnippet-capf #'cape-keyword #'cape-dabbrev))
;;  (add-to-list 'completion-at-point-functions #'cape-file)
;;  (add-to-list 'completion-at-point-functions #'gopar/cape-yasnippet-keyword-dabbrev)
;;  (add-to-list 'completion-at-point-functions #'gopar/cape-dict-only-in-strings)
;;  (add-to-list 'completion-at-point-functions #'gopar/cape-dict-only-in-comments))
)
;; not sure we need it
;;(use-package orderless
;;  :ensure t
;;  :after consult
;;  :custom
;;  (completion-styles '(orderless basic initials flex))
;;  (completion-category-overrides '((file (styles basic partial-completion)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check syntax
(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode))

;; insert matching delimiters
(electric-pair-mode t)

;; Used to works in all codebase
(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<") ;; "C-+"
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GPT like ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gptel
(use-package gptel
  :ensure t
  :init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File extension specific ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; json
(use-package json-mode
  :ensure
  :init)
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

;;csv
(use-package csv-mode
  :ensure t
  :config
  :hook (
	 (csv-mode . csv-guess-set-separator)
         (csv-mode . csv-align-mode)
	 )
  )
;;markdown
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

;; pdf : have to M-x pdt-tools-install each time on w32
;; (use-package pdf-tools
;;   :demand t
;;   :init
;;   (pdf-tools-install)
;;   :config
;;   (add-hook 'pdf-isearch-minor-mode-hook (lambda () (ctrlf-local-mode -1)))
;;   (use-package org-pdftools
;;     :hook (org-mode . org-pdftools-setup-link)))
;;(pdf-tools-install)
;;(add-to-list 'auto-mode-alist '("\\.[pP][dD][fF]\\'" . pdf-view-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help / doc / keybing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interactively show help/doc/annotation
(use-package marginalia
  :ensure
  :init
  (marginalia-mode))

;; keybind
(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))
(global-set-key (kbd "C-c C-l") 'reload-init-file)
(global-set-key (kbd "C-c ;") 'comment-region)
(global-set-key (kbd "C-c .") 'uncomment-region)
;; copy/paste with C-c C-v ; C-k C-y always working ; do NOT use (cua-mode t), it will break C-z
(global-set-key (kbd "C-c SPC") 'copy-region-as-kill)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-x SPC") 'kill-region)

;; cheat sheet
(use-package cheatsheet
  :ensure t
  :config
  (cheatsheet-add-group 'Function-key
                        '(:key "<F3>"  :description "previous Tab")
                        '(:key "<F4>"  :description "next Tab")
                        '(:key "<F5>"  :description "show/hide Neotree")
                        '(:key "<F6>"  :description "open magit")
                        '(:key "<F10>" :description "open menu bar")
                        '(:key "C-<F1>" :description "open cheatsheet"))
  (cheatsheet-add-group 'Tab
                        '(:key "C-x t f" :description "open file in a new tab")
			'(:key "C-x t 0" :description "close current tab")
			'(:key "C-x t r" :description "rename tab")
                        '(:key "<F4>" :description "next tab")
                        '(:key "<F5>" :description "previous tab"))
  (cheatsheet-add-group 'Window
                        '(:key "M-0 9" :description "switch to window 0 .. 9")
			'(:key "C-x o" :description "next window")
                        '(:key "C-x 1" :description "close all windows except current one")
                        '(:key "C-x 2" :description "split horizontally")
                        '(:key "C-x 3" :description "split vertically")
                        '(:key "C-x 0" :description "close current window"))
  (cheatsheet-add-group 'Bookmark
                        '(:key "C-x r m" :description "mark file in bookmark")
                        '(:key "C-x r l" :description "list bookmark")
                        '(:key "M-x bookmark-save" :description "save bookmark"))
  (cheatsheet-add-group 'Magit
                        '(:key "<F6>" :description "open magit")
                        '(:key "C-c C-c" :description "commit")
                        '(:key "s" :description "stage")
                        '(:key "p" :description "push")
                        '(:key "M-x magit-pull" :description "pull"))
  (cheatsheet-add-group 'Code
			'(:key "C-c ;" :description "comment region")
			'(:key "C-c ." :description "uncomment region")
			'(:key "ESC-%" :description "query replace")
			;; tks to consult
			'(:key "M-g i" :description "function reference")
			'(:key "M-s l" :description "search")
			'(:key "M-s g" :description "grep in code")
                        '(:key "M-g M-g" :description "goto line")))
  (cheatsheet-add-group 'Emacs
                        '(:key "C-c SPC" :description "copy")
                        '(:key "C-v" :description "paste")
                        '(:key "C-x SPC" :description "cut")
                        '(:key "C-x f" :description "open file")
                        '(:key "C-x C-s" :description "save file")
                        '(:key "C-s" :description "search")
                        '(:key "C-SPC" :description "start selection")
                        '(:key "C-:" :description "undo")
                        '(:key "C-h b" :description "describe bindings")
			'(:key "C-c C-l" :description "reload init file"))
(global-set-key (kbd "<C-f1>") 'cheatsheet-show)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode and reveal
;; pre requisite : cd ~/git ; git clone --recursive https://gitlab.com/oer/emacs-reveal.git
;; conf works but i dont really like revveal
;; removed all elpa and restart !
;; maybe have a look to https://systemcrafters.net/emacs-tips/presentations-with-org-present/
;;
;;(use-package bibtex
;;  :ensure t)
;;(use-package bibtex-completion
;;  :ensure t)
;;(use-package helm-bibtex
;;  :ensure t)
;;(setq bibtex-completion-bibliography '("~/bibtext/test.bib")
;;	bibtex-completion-library-path '("~/bibtext/")
;;	bibtex-completion-notes-path "~/bibtext/"
;;	bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"
;;	bibtex-completion-additional-search-fields '(keywords)
;;	bibtex-completion-display-formats
;;	'((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
;;	  (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
;;	  (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
;;	  (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
;;	  (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
;;	bibtex-completion-pdf-open-function
;;	(lambda (fpath)
;;	  (call-process "open" nil 0 nil fpath)))
;;(setq bibtex-autokey-year-length 4
;;      bibtex-autokey-name-year-separator "-"
;;      bibtex-autokey-year-title-separator "-"
;;      bibtex-autokey-titleword-separator "-"
;;      bibtex-autokey-titlewords 2
;;      bibtex-autokey-titlewords-stretch 1
;;      bibtex-autokey-titleword-length 5)
;;(define-key bibtex-mode-map (kbd "H-b") 'org-ref-bibtex-hydra/body)
;;(use-package org-ref
;;  :ensure t)
;;(require 'org-ref-helm)
;;
;; WARNING !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; on fresh install, need to comment next 2 lines
;; then edit ~/.emacs.d/elpa/org-ref-2.0.0/org-ref-helm-bibtex.el and comment (require 'helm-config)
;; then uncomment and restart emacs
;;(add-to-list 'load-path "~/git/emacs-reveal")
;;(require 'emacs-reveal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tramp
;; working with /plink:user@host/
;;(setq tramp-verbose 6)
;;(when (eq system-type 'windows-nt)
;;  (setq tramp-default-method "plink")
;;  (setq tramp-auto-save-directory "c:\\temp"))
;;  (when (and (not (string-match putty-directory (getenv "PATH")))
;;	     (file-directory-p putty-directory))
;;    (setenv "PATH" (concat putty-directory ";" (getenv "PATH")))
;;    (add-to-list 'exec-path putty-directory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; w32 specific
;; all-the-icons-install-fonts then go to font path and instal fonts
;; idkhow, git is working with magit on w32 :)
;; got it with magit-pull from l64
;; C:\Users\USERNAME\AppData\Roaming>mklink /h. emacs ..\..\git\rc\.emacs
;; settings to set working dir is nit working, so made a link to runemacs.exe and set working dir in it
;;(when (eq system-type 'windows-nt)
;;(setq default-directory "C:\Users\creti\git")
;;(cd "C:\Users\creti\git")
;; more to know, we need https://gnuwin32.sourceforge.net/packages/diffutils.htm in chocolatey bin to run magit on w32
;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EO Emacs config
(provide '.emacs)
