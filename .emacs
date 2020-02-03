(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("85968e61ff2c490f687a8159295efb06dd05764ec37a5aef2c59abbd485f0ee4" "2a9039b093df61e4517302f40ebaf2d3e95215cb2f9684c8c1a446659ee226b9" "7f89ec3c988c398b88f7304a75ed225eaac64efa8df3638c815acc563dfd3b55" default)))
 '(org-agenda-files (quote ("~/org/work.org")))
 '(package-selected-packages
   (quote
    (column-enforce-mode evil-org git-auto-commit-mode magit powerline recover-buffers darktooth-theme flycheck ## gruvbox-theme evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; ;; Download Evil
; (unless (package-installed-p 'evil)
;   (package-install 'evil))

; EVIL MODE CONFIG
(setq-default evil-want-C-u-scroll t) ; remap C-u to page up THIS MUST BE BEFORE require evil
(require 'evil)
(evil-mode 1)
(setq-default evil-escape-key-sequence "kj")
(setq-default evil-escape-delay 0.2)
(evil-escape-mode 1)

(evil-define-key 'normal org-mode-map
  (kbd "TAB") 'org-cycle
 ; ">" 'org-shiftmetaright
 ; "<" 'org-shiftmetaleft
)

(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

(setq evil-want-C-i-jump nil) ; fix for evil-org TAB issue https://github.com/Somelauw/evil-org-mode#common-issues

; add proselint and enable it in org-mode
(add-hook 'after-init-hook #'global-flycheck-mode)

; don't truncate lines
(global-visual-line-mode t)

; enable spell check in org-mode
(dolist (hook '(text-mode-hook org-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))

; magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

; powerline
(require 'powerline)
(powerline-default-theme)

; archive done tasks in org-mode
; https://stackoverflow.com/a/27043756/1576860
(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'agenda)) ;applies to all agenda files. Use 'file' for the current file. 'tree' for the current subtree

; set the default capture file
(setq org-default-notes-file (concat org-directory "/capture.org"))
(define-key global-map "\C-cc" 'org-capture)

; mark characters over the 80th column globally
; https://github.com/jordonbiondo/column-enforce-mode/
(global-column-enforce-mode t)

; theme
(load-theme 'gruvbox t)

(provide '.emacs)
;;; .emacs ends here
