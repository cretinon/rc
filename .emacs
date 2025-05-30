;; package --- summary

;;; Code:

;;; Commentary:

(require 'package)
(setq package-enable-at-startup nil)

;; https://emacs.stackexchange.com/a/2989
(setq package-archives
      '(("elpa"     . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("melpa-stable" . 10)
        ("elpa"     . 5)
        ("melpa"        . 0)))

(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; see https://github.com/gopar/.emacs.d/blob/main/README.org

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package golden-ratio
  :ensure t
  :hook (after-init . golden-ratio-mode)
  :custom
  (golden-ratio-exclude-modes '(occur-mode)))

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

(use-package dabbrev
  :defer t
  :custom
  (dabbrev-upcase-means-case-search t)
  (dabbrev-check-all-buffers nil)
  (dabbrev-check-other-buffers t)
  (dabbrev-friend-buffer-function 'dabbrev--same-major-mode-p)
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package corfu
  :ensure t
  ;; Originally, I liked the idea of `corfu-send` but this makes it behave
  ;; in way that is different from 'fish' shell. So lets disable and see
  ;; how we feel about it in the future
  ;; :bind (:map corfu-map
  ;;             ("RET" . corfu-send))
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-on-exact-match 'insert) ;; Insert when there's only one match
  (corfu-quit-no-match t)        ;; Quit when ther is no match
  :init
  (global-corfu-mode)

  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input)
                (eq (current-local-map) read-passwd-map))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1))

(use-package cape
  :ensure t
  :bind ("C-c SPC" . cape-dabbrev)
  :custom
  (cape-dict-case-replace nil)
  (cape-dabbrev-buffer-function 'cape-same-mode-buffers)

  :init
  (defun gopar/cape-dict-only-in-comments ()
    (cape-wrap-inside-comment 'cape-dict))

  (defun gopar/cape-dict-only-in-strings ()
    (cape-wrap-inside-string 'cape-dict))

  (defun gopar/cape-yasnippet-keyword-dabbrev ()
    (cape-wrap-super #'yasnippet-capf #'cape-keyword #'cape-dabbrev))

  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'gopar/cape-yasnippet-keyword-dabbrev)
  (add-to-list 'completion-at-point-functions #'gopar/cape-dict-only-in-strings)
  (add-to-list 'completion-at-point-functions #'gopar/cape-dict-only-in-comments))

(use-package vertico
  :ensure t
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :init
  (vertico-mode))
;;  (setopt vertico-cycle t))

(use-package orderless
  :ensure t
  :after consult
  :custom
  (completion-styles '(orderless basic initials flex))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;; how-to use magit :
;; pre requisite : set username for repo : git config user.name "jacques@cretinon.fr"
;;
;; open : "C-x g"
;; git add : Once file is saved, in buffer type "M-x magit-stage-file"
;; git rm :
;; git commit :
;; * move line selector on modified file(s) in "Unstaged changes"
;; * type 's' (in order to set to "staged changes")
;; * type 'c' 'c' (in order to commit)
;; * edit changelog message
;; * type "C-c C-c"
;; git push :
;; * move line selector on Unmerged into branched
;; * type 'P' (in order to push) and 'p' again

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

;; Part of magit
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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(manoj-dark))
 '(package-selected-packages '(git-gutter vertico golden-ratio)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Put backup files neatly away --
(let ((backup-dir "~/tmp/emacs/backups")
      (auto-saves-dir "~/tmp/emacs/auto-saves/"))
  (dolist (dir (list backup-dir auto-saves-dir))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keybind
(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))

(global-set-key (kbd "C-c C-l") 'reload-init-file)

;; useful emacs keybinding below
;; "C-h b" describes bindings
;; "C-x C-s" save buffer
;; "C-x 1" show only this buffer
;; "C-x 2" split horizontally
;; "C-x 3" split vertically
;; "C-x 0" kill buffer and close it
;; "C-x o" switch buffer
;; "C-_" undo
;; "C-/" undo
;; "C-s" search
;; "Esc-%" Query replace
;; "C-space" Start selection
;; "C-x r m" set bookmark
;; "C-x r l" list bookmarks
;; "M-x bookmark-save" save bookmarks

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; redifine key using emacs in putty. See doc here : https://www.emacswiki.org/emacs/PuTTY#toc8
(if (eq system-uses-terminfo t)
    (progn                              ;; PuTTY hack - needs to be in SCO mode
      (define-key key-translation-map [\e] [\M])
      (define-key input-decode-map "\e[H" [home])
      (define-key input-decode-map "\e[F" [end])
      (define-key input-decode-map "\e[D" [S-left])
      (define-key input-decode-map "\e[C" [S-right])
      (define-key input-decode-map "\e[A" [S-up])
      (define-key input-decode-map "\e[B" [S-down])
      (define-key input-decode-map "\e[C" [S-right])
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode <-> file extension
(use-package json-mode
  :ensure
  :init)

(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
(use-package cyberpunk-theme
  :ensure
  :init)

(load-theme 'cyberpunk t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EO Emacs config
(provide '.emacs)
;;; .emacs ends here
