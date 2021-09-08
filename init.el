(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (java . t)
   (python . t)
   (shell . t)))

(setq org-confirm-babel-eveluate nil)

(use-package general
  :config
  (general-create-definer cxr/leader-keys
                          :keymaps '(normal insert visual emacs)
                          :prefix "SPC"
                          :global-prefix "C-SPC")
  (general-auto-unbind-keys))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(cxr/leader-keys
 "t"     '(:ignore t :which-key "toggles")
 "b"     '(:ignore t :which-key "buffers")
 "bb"    '(counsel-switch-buffer              :which-key "switch buffer")
 "b TAB" '(evil-switch-to-windows-last-buffer :which-key "switch to last buffer")
 )

(set-face-attribute 'default nil :font "Hack" :height 140)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-palenight t)
  ;;(load-theme 'doom-outrun-electric t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Don't show the splash screen
(setq inhibit-startup-message t)

;; Start emacs window maximised
;; the t parameter apends to the hook, instead of prepending
;; this means it'd be run after other hooks that might fiddle
;; with the frame size
;; https://emacsredux.com/blog/2020/12/04/maximize-the-emacs-frame-on-startup/
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

(tool-bar-mode -1)      ; Disable the chunky toolbar
(tooltip-mode -1)       ; Disable tooltips
(menu-bar-mode -1)      ; Disable the top menu bar

                                        ; these don't seem to work in terminal mode
                                        ;(scroll-bar-mode -1)    ; Disable visible scrollbar
                                        ;(set-fringe-mode 25)    ; Add left and right margins

(setq visible-bell t)    ; Stop beeping at me!
(use-package all-the-icons)
;; Run ~M-x all-the-icons-install-fonts~ after first setup to
;; install icon fonts

(column-number-mode t) ; Show column number in mode line

(global-display-line-numbers-mode t)

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

                                        ;(use-package prism)

;; https://github.com/seagle0128/doom-modeline
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 5))

(set-face-attribute 'mode-line nil :family "Hack" :height 130)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; needed for evil undo. There's a built-in in emacs 28 I should check out
(use-package undo-tree
  :init
  (global-undo-tree-mode 1))

;; https://evil.readthedocs.io/en/latest/settings.html
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-tree)
  (setq evil-want-C-u-scroll t)
  (setq evil-escape-key-sequence "kj")
  (setq evil-escape-delay 0.2)
  :config
  (evil-mode 1)
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  )

;; https://github.com/emacs-evil/evil-collection
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; https://github.com/syl20bnr/evil-escape
(use-package evil-escape
  :after evil
  :ensure t
  :init
  (setq-default evil-escape-key-sequence "kj")
  (setq-default evil-escape-delay 0.2))
:config
(evil-escape-mode 1)

;; Installs Ivy, Counsel and Swiper
(use-package counsel
  :diminish
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-s" . swiper)
         ("C-r" . 'counsel-minibuffer-history)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; https://github.com/Yevgnen/ivy-rich
(use-package ivy-rich
  :init (ivy-rich-mode 1)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  )

(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key)
  )

(use-package org
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t))

;; https://github.com/integral-dw/org-superstar-mode
(use-package org-superstar
  :config
  (org-superstar-mode 1))

;; org-mode leader keys
(cxr/leader-keys
 "o"     '(:ignore t :which-key "org")
 "oR"    '(org-mode-restart :which-key "restart"))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t) ; don't warn me about v2 migration
  :custom
  (org-roam-directory "~/org/roam")
  :config
  (org-roam-db-autosync-mode t))

;; org-roam leader keys
(cxr/leader-keys
 "or"  '(:ignore t            :which-key "roam")
 "orc" '(org-roam-capture     :which-key "capture")
 "orf" '(org-roam-node-find   :which-key "find node")
 "ori" '(org-roam-node-insert :which-key "insert node"))
