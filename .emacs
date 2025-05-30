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
  ;; takes too long to update on first try
  ;; (neo-vc-integration '(face char))
  (neo-show-hidden-files nil)
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
;;
;; open : "C-x g"
;; git add :
;; git rm :
;; git commit :
;; * move line selector on modified file(s) in "Unstaged changes"
;; * type 's' (in order to set to "staged changes")
;; * type 'c' 'c' (in order to commit)
;; * edit changelog message
;; * type "C-c c"
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

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))

(global-set-key (kbd "C-c C-l") 'reload-init-file)    ; Reload .emacs file

(use-package cyberpunk-theme
  :ensure
  :init)

(load-theme 'cyberpunk t)

(provide '.emacs)
;;; .emacs ends here






