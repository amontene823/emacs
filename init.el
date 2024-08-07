(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(setq visible-bell t)
;; fonts
(set-face-attribute 'default nil :font "Fira Code Retina" :height 150)
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 150)
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 150 :weight 'regular)
(column-number-mode)
(global-display-line-numbers-mode t)
;; Disable line numbers for some modes
(defun disable-line-numbers ()
  "Hook to disable line-number-mode"
  (display-line-numbers-mode 0))
(dolist (hook `(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook hook #'disable-line-numbers))

;; doom
(use-package doom-themes)
:config
(load-theme 'doom-1337 t)
(use-package doom-modeline
  :custom (doom-modeline-height 15)
  :config (doom-modeline-mode 1))

;; raindbow parantheses
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package all-the-icons)
  ;;:if (display-graphic-p)
  ;; run on first install
  ;; M-x all-the-icons-install-fonts
  ;; M-x nerd-icons-install-fonts

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
:config
(general-create-definer am/leader-keys
:keymaps '(normal insert visual emacs)
:prefix "SPC"
:global-prefix "C-SPC")
(am/leader-keys
"t"  '(:ignore t :which-key "Toggles")
"tt" '(counsel-load-theme :which-key "Choose Theme")

"b"  '(:ignore b :which-key "Buffer")
"bb" '(next-buffer :which-key "Next")
"bn" '(next-buffer :which-key "Next")
"bp" '(previous-buffer :which-key "Previous")
"bN" '(previous-buffer :which-key "Previous")
"bl" '(counsel-switch-buffer :which-key "Switch")
"bk" '(kill-buffer :which-key Kill)

"w"  '(:ignore w :which-key "Window")
"ww" '(evil-window-next :which-key "Next")
"wn" '(evil-window-next :which-key "Next")
"wN" '(evil-window-prev :which-key "Previous")
"ws" '(evil-window-split :which-key "Horizontal Split")
"wv" '(evil-window-vsplit :which-key "Vertical Split")
"wc" '(evil-window-delete :which-key "Close")

":" '(counsel-M-x :which-key "M-x")

"h"  '(:ignore h :which-key "Help")
"hv" '(counsel-describe-variable :which-key "Describe Variable")
"hf" '(counsel-describe-function :which-key "Describe Function")
"hi" '(indent-region :which-key "Indent Region")

"f"  '(:ignore f :which-key "Files")
"fr" '(counsel-recentf :while-key "Recent Files")
"ff" '(find-file :while-key "Find File")
))

;; modeline completion
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
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
;; Key bindings and documentation string
(use-package ivy-rich
  :config
  (ivy-rich-mode 1))

(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0)
  (which-key-mode))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  ;;(setq evil-want-C-u-scroll t)
  ;;(setq evil-want-C-i-jump nil)
  ;;:hook (evil-mode . am/evil-hook)
  :config
  (evil-mode 1)
  ;;(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;;(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)

  (evil-set-undo-system 'undo-redo)) ;; undo-redo functionality

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(defun am/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.4)
  		  (org-level-2 . 1.2)
  		  (org-level-3 . 1.1)
  		  (org-level-4 . 1.0)
  		  (org-level-5 . 1.0)
  		  (org-level-6 . 1.0)
  		  (org-level-7 . 1.0)
  		  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))
  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun am/org-mode-setup ()
(org-indent-mode)
(variable-pitch-mode 1)
(visual-line-mode 1))

(use-package org
  :hook
  (org-mode . am/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
  	org-hide-emphasis-markers nil
  	org-agenda-files
  	'("~/org"))
  (auto-revert-mode 1)
  (am/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package magit)
(use-package transient)

(use-package org-roam
  :init
  (setq org-roam-vs-ack t)
  :custom
  (org-roam-directory (file-truename "~/org/roam/"))
  (org-roam-db-autosync-mode)
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 :map org-mode-map
	 ("C-M-i" . completion-at-point))
  :config
  (org-roam-setup))

(use-package vterm)

;; Org babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (shell . t)))
(setq org-confirm-babel-evaluate nil)

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
