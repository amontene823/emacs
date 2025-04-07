;; -*- lexical-binding: t; -*-

;; (require 'package)
;; (setq package-archives '(("melpa" . "https://melpa.org/packages/")
;;                          ("org" . "https://orgmode.org/elpa/")
;;                          ("elpa" . "https://elpa.gnu.org/packages/")))
;; (package-initialize)
;; (unless package-archive-contents
;;   (package-refresh-contents))
;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))
;; (require 'use-package)
;; (setq use-package-always-ensure t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage)) 

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(setq visible-bell t)
;; fonts
(set-face-attribute 'default nil :font "Fira Code Retina" :height 140 :weight 'medium)
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 140 :weight 'medium)
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 140 :weight 'medium)
(column-number-mode)
(global-display-line-numbers-mode t)
;; Disable line numbers for some modes
(defun disable-line-numbers ()
  "Hook to disable line-number-mode"
  (display-line-numbers-mode 0))
(dolist (hook `(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook
		pdf-view-mode-hook))
  (add-hook hook #'disable-line-numbers))

;; doom
(use-package doom-themes)
:config
;; (load-theme 'doom-1337 t)
(load-theme 'modus-operandi t)
(use-package doom-modeline
  :custom (doom-modeline-height 15)
  :config (doom-modeline-mode 1))

;; raindbow parantheses
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package all-the-icons)

(global-unset-key (kbd "C-/"))
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package evil
  :init
  ;; these settings were important for correcting indentation in
  ;; org mode src blocks
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  ;;(define-key evil-normal-state-map (kbd "C-.") nil)
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

 (use-package evil-nerd-commenter
   :after evil
   :bind
   (("C-/" . evilnc-comment-or-uncomment-lines)))

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(when (eq system-type 'darwin) ;; Check if the OS is macOS
  (eval-after-load "flyspell"
    '(progn
       (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
       (define-key flyspell-mouse-map [mouse-3] #'undefined))))

;; modeline completion
;; (use-package ivy
;;   :diminish
;;   :bind (("C-s" . swiper)
;;          :map ivy-minibuffer-map
;;          ("TAB" . ivy-alt-done)
;;          ("C-l" . ivy-alt-done)
;;          ("C-j" . ivy-next-line)
;;          ("C-k" . ivy-previous-line)
;;          :map ivy-switch-buffer-map
;;          ("C-k" . ivy-previous-line)
;;          ("C-l" . ivy-done)
;;          ("C-d" . ivy-switch-buffer-kill)
;;          :map ivy-reverse-i-search-map
;;          ("C-k" . ivy-previous-line)
;;          ("C-d" . ivy-reverse-i-search-kill))
;;   :config
;;   (ivy-mode 1))
;; ;; (use-package ivy-bibtex
;; ;;   :after (ivy)
;;   :custom
;;   (bibtex-completion-bibliography '("~/org/references/bibfile.bib"))
;;   (bibtex-completion-library-path '("~/org/references")))

;; (use-package org-ref
;;   :after ivy-bibtex
;;   :custom
;;   (reftex-default-bibliography '("~/org/references/bibfile.bib"))
;;   ;;(org-ref-bibliography-notes "~/org/references/notes.org")
;;   (org-ref-default-bibliography '("~/org/references/bibfile.bib"))
;;   (org-ref-pdf-directory "~/org/references/"))
;; (require 'org-ref-ivy)

   ;; ;; Key bindings and documentation string
   ;; (use-package ivy-rich
   ;;   :config
   ;;   (ivy-rich-mode 1))


   ;; (use-package counsel
   ;;   :bind (("M-x" . counsel-M-x)
   ;;          ("C-x b" . counsel-ibuffer)
   ;;          ("C-x C-f" . counsel-find-file)
   ;;          :map minibuffer-local-map
   ;;          ("C-r" . 'counsel-minibuffer-history))
   ;;   :config
   ;;   (setq ivy-initial-inputs-alist nil))

(use-package vertico
    ;; :custom
    ;; (vertico-scroll-margin 0) ;; Different scroll margin
    ;; (vertico-count 20) ;; Show more candidates
    ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
    ;;(vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
    :init
    (vertico-mode))

  (with-eval-after-load 'vertico
    (define-key vertico-map (kbd "C-j") 'vertico-next)
    (define-key vertico-map (kbd "C-k") 'vertico-previous))

  (use-package vertico-directory
    :straight nil
    :after vertico
    ;; More convenient directory navigation commands
    :bind (:map vertico-map
                ("RET" . vertico-directory-enter)
                ("DEL" . vertico-directory-delete-char)
                ("M-DEL" . vertico-directory-delete-word))
    ;; Tidy shadowed file names
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

  (use-package corfu
    ;; Optional customizations
    ;; :custom
    ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
    ;; (corfu-auto t)                 ;; Enable auto completion
    ;; (corfu-separator ?\s)          ;; Orderless field separator
    ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
    ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
    ;; (corfu-preview-current nil)    ;; Disable current candidate preview
    ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
    ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
    ;; (corfu-scroll-margin 5)        ;; Use scroll margin

    ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
    ;; :hook ((prog-mode . corfu-mode)
    ;;        (shell-mode . corfu-mode)
    ;;        (eshell-mode . corfu-mode))

    ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
    ;; be used globally (M-/).  See also the customization variable
    ;; `global-corfu-modes' to exclude certain modes.
    :bind
    (:map corfu-map ("SPC" . corfu-insert-separator))
    :init
    (global-corfu-mode))

  (use-package emacs
    :straight (:type built-in)
    :custom
    ;; TAB cycle if there are only few candidates
    ;; (completion-cycle-threshold 3)

    ;; Enable indentation+completion using the TAB key.
    ;; `completion-at-point' is often bound to M-TAB.
    (tab-always-indent 'complete)

    ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
    ;; try `cape-dict'.
    ;; (text-mode-ispell-word-completion nil)

    ;; Support opening new minibuffers from inside existing minibuffers.
    (enable-recursive-minibuffers t)
    ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
    ;; mode.  Vertico commands are hidden in normal buffers. This setting is
    ;; useful beyond Vertico.
    (read-extended-command-predicate #'command-completion-default-include-p)
    :init
    ;; Add prompt indicator to `completing-read-multiple'.
    ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
    (defun crm-indicator (args)
      (cons (format "[CRM%s] %s"
                    (replace-regexp-in-string
                     "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                     crm-separator)
                    (car args))
            (cdr args)))
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

    ;; Do not allow the cursor in the minibuffer prompt
    (setq minibuffer-prompt-properties
          '(read-only t cursor-intangible t face minibuffer-prompt))
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

  ;; makes scrolling good in emacs-mac
  (use-package ultra-scroll-mac
    :straight (ultra-scroll-mac :type git :host github :repo "jdtsmith/ultra-scroll-mac")
    :if (eq window-system 'mac)
    :init
    (setq scroll-conservatively 101 ; important!
          scroll-margin 0) 
    :config
    ;; Enable the ultra-scroll mode
    (ultra-scroll-mac-mode 1))

  ;; (use-package orderless
  ;;   :custom
  ;;   ;; Configure a custom style dispatcher (see the Consult wiki)
  ;;   ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;;   ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  ;;   (completion-styles '(orderless basic))
  ;;   (completion-category-defaults nil)
  ;;   (completion-category-overrides '((file (styles partial-completion)))))

(use-package orderless
  :init
  ;; Tune the global completion style settings to your liking!
  ;; This affects the minibuffer and non-lsp completion at point.
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil))


  (use-package consult
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
           ;; ("C-f"   . consult-ripgrep)
           ("M-s g" . consult-grep)
           ("M-s G" . consult-git-grep)
           ("C-l" . consult-line)
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

    ;; Enable automatic preview at point in the *Completions* buffer. This is
    ;; relevant when you use the default completion UI.
    :hook (completion-list-mode . consult-preview-at-point-mode)

    ;; The :init configuration is always executed (Not lazy)
    :init

    ;; Optionally configure the register formatting. This improves the register
    ;; preview for `consult-register', `consult-register-load',
    ;; `consult-register-store' and the Emacs built-ins.
    (setq register-preview-delay 0.5
          register-preview-function #'consult-register-format)

    ;; Optionally tweak the register preview window.
    ;; This adds thin lines, sorting and hides the mode line of the window.
    (advice-add #'register-preview :override #'consult-register-window)

    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)

    ;; Configure other variables and modes in the :config section,
    ;; after lazily loading the package.
    :config
    (recentf-mode) ;;turns on recent-f mode so consult can find recently opened files

    ;; Optionally configure preview. The default value
    ;; is 'any, such that any key triggers the preview.
    ;; (setq consult-preview-key 'any)
    ;; (setq consult-preview-key "M-.")
    ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
    ;; For some commands and buffer sources it is useful to configure the
    ;; :preview-key on a per-command basis using the `consult-customize' macro.
    (consult-customize
     consult-theme :preview-key '(:debounce 0.2 any)
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-bookmark consult--source-file-register
     consult--source-recent-file consult--source-project-recent-file
     ;; :preview-key "M-."
     :preview-key '(:debounce 0.4 any)))

  ;; Optionally configure the narrowing key.
  ;; Both "<" and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)

  (use-package marginalia
    :config
    (marginalia-mode))

  (use-package embark
    :bind
    (("C-;" . embark-dwim)        ;; good alternative: M-.
     ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
    :init
    ;; Optionally replace the key help with a completing-read interface
    (setq prefix-help-command #'embark-prefix-help-command)

    ;; Show the Embark target at point via Eldoc. You may adjust the
    ;; Eldoc strategy, if you want to see the documentation from
    ;; multiple providers. Beware that using this can be a little
    ;; jarring since the message shown in the minibuffer can be more
    ;; than one line, causing the modeline to move up and down:

    ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
    ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
    :config
    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none)))))
  ;; Consult users will also want the embark-consult package.
  (use-package embark-consult
    :hook
    (embark-collect-mode . consult-preview-at-point-mode))

(use-package auctex
    ;;:defer t
    :hook ((LaTeX-mode . LaTeX-preview-setup)
           (LaTeX-mode . turn-on-reftex)   ;; Enable RefTeX for cross-referencing
           (LaTeX-mode . flyspell-mode)    ;; Enable Flyspell for spell checking
           (LaTeX-mode . LaTeX-math-mode)) ;; Enable LaTeX Math mode
    :init
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq-default TeX-master nil)         ;; Ask for master file when opening a new TeX file
    (setq TeX-PDF-mode t)
    (setq TeX-view-program-selection '((output-pdf "Pscrollools"))
          TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
          LaTeX-command-style '(("" "%(PDF)%(latex) --synctex=1 %(file-line-error) %(extraopts) %(output-dir) %S%(PDFout)")) ;; synctex for TeX from/to PDF jumping

          TeX-source-correlate-start-server t) ;; not sure if last line is neccessary
    (setq TeX-source-correlate-method 'synctex) ; enable synctex
    (setq TeX-source-correlate-mode t)) ; enable text-source-correlate using synctex

(defun my-custom-function ()
  "Automatically run `TeX-command-run-all` when a LaTeX file is saved."
  (TeX-command-run-all nil))

(defun add-latex-save-hook ()
  "Add a save hook to compile LaTeX files."
  (add-hook 'after-save-hook 'my-custom-function nil t))  ;; Buffer-local hook

(add-hook 'LaTeX-mode-hook 'add-latex-save-hook)

  ;; (defun my-custom-function ()
    ;; (TeX-command-run-all nil))
  ;; (add-hook 'after-save-hook 'my-custom-function)

  ;; (use-package latex-preview-pane
  ;; :after auctex
  ;; :config
  ;; (latex-preview-pane-enable)
  ;; :hook (LaTeX-mode . latex-preview-pane-mode))

(defun am/org-font-setup ()
    ;; Replace list hyphen with dot
    ;; (font-lock-add-keywords 'org-mode
                            ;; '(("^ *\\([-]\\) "
                               ;; (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
    ;; Set faces for heading levels
    (dolist (face '((org-level-1 . 1.6)
                    (org-level-2 . 1.4)
                    (org-level-3 . 1.2)
                    (org-level-4 . 1.0)
                    (org-level-5 . 1.0)
                    (org-level-6 . 1.0)
                    (org-level-7 . 1.0)
                    (org-level-8 . 1.0)))
      (set-face-attribute (car face) nil :font "Cantarell" :weight 'bold :height (cdr face)))
    ;; Ensure that anything that should be fixed-pitch in Org files appears that way
    (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-column nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-column-title nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

  (defun am/org-setup()
    ;; Add frame borders and window dividers
    (modify-all-frames-parameters
     '((right-divider-width . 40)
       (internal-border-width . 40)))
    (dolist (face '(window-divider
                    window-divider-first-pixel
                    window-divider-last-pixel))
      (face-spec-reset-face face)
      (set-face-foreground face (face-attribute 'default :background)))
    (set-face-background 'fringe (face-attribute 'default :background))

    (setq
     ;; Edit settings
     org-auto-align-tags nil
     org-tags-column 0
     org-catch-invisible-edits 'show-and-error
     org-special-ctrl-a/e t
     org-insert-heading-respect-content t
     line-spacing 0.1

     ;; Org styling, hide markup etc.
     org-hide-emphasis-markers t
     org-pretty-entities t
     org-pretty-entities-include-sub-superscripts nil

     ;; Agenda styling
     org-agenda-tags-column 0
     org-agenda-block-separator ?─
     org-agenda-time-grid
     '((daily today require-timed)
       (800 1000 1200 1400 1600 1800 2000)
       " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
     org-agenda-current-time-string
     "◀── now ─────────────────────────────────────────────────")

    ;; Ellipsis styling
    (setq org-ellipsis "…")
    (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)
    (set-face-attribute 'org-table nil :inherit 'fixed-pitch) )




  (defun am/org-mode-setup ()
    ;; (org-indent-mode 1)
    (variable-pitch-mode 1)
    (visual-line-mode 1))

  (use-package org
    ;; :straight (:type built-in) 
    :hook
    (org-mode . am/org-mode-setup)
    ;; (org-src-mode-hook . company-mode)
    :config
    (setq org-hide-emphasis-markers nil
    org-image-actual-width nil
  	org-adapt-indentation t
  	org-startup-indented t
  	org-agenda-files
  	'("~/org"))
    ;; (setq org-blank-before-new-entry
  	;; '((heading . nil)
            ;; (plain-list-item . auto)))
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "ORDERED(o)" "|" "DONE(d)" "CANCELLED(c)" "RECEIVED(r)")))
(setq org-hierarchical-todo-statistics nil)
  (auto-revert-mode 1)
  (am/org-font-setup)
  (am/org-setup))

  (use-package org-modern
    :after org
    :hook (org-mode . global-org-modern-mode)
    :custom
    (org-modern-star 'replace)
    (org-modern-timestamp nil)) 
    
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

    ;; Adjusts org latex font
    (setq org-format-latex-options '(:foreground default :background default :scale 1.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

    (use-package cdlatex
      :hook (org-mode . turn-on-org-cdlatex))

    (use-package org-fragtog
      :hook (org-mode . org-fragtog-mode))

    (use-package org-noter
      :after org-noter-pdftools)

    (use-package org-pdftools
      :hook (org-mode . org-pdftools-setup-link))

    (use-package org-noter-pdftools
      :after org-noter
      :config
      ;; Add a function to ensure precise note is inserted
      (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
        (interactive "P")
        (org-noter--with-valid-session
         (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                       (not org-noter-insert-note-no-questions)
                                                     org-noter-insert-note-no-questions))
               (org-pdftools-use-isearch-link t)
               (org-pdftools-use-freepointer-annot t))
  	 (org-noter-insert-note (org-noter--get-precise-info)))))

      ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
      (defun org-noter-set-start-location (&optional arg)
        "When opening a session with this document, go to the current location.
       With a prefix ARG, remove start location."
        (interactive "P")
        (org-noter--with-valid-session
         (let ((inhibit-read-only t)
               (ast (org-noter--parse-root))
               (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
  	 (with-current-buffer (org-noter--session-notes-buffer session)
             (org-with-wide-buffer
              (goto-char (org-element-property :begin ast))
              (if arg
  		(org-entry-delete nil org-noter-property-note-location)
                (org-entry-put nil org-noter-property-note-location
                               (org-noter--pretty-print-location location))))))))
      (with-eval-after-load 'pdf-annot
        (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

    (use-package org-roam-ui
      :straight
      (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
      :after org-roam
      ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
      ;;         a hookable mode anymore, you're advised to pick something yourself
      ;;         if you don't care about startup time, use
      ;;  :hook (after-init . org-roam-ui-mode)
      :config
      (setq org-roam-ui-sync-theme t
            org-roam-ui-follow t
            org-roam-ui-update-on-save t
            org-roam-ui-open-on-start t))

(use-package citar
    :custom
    (citar-bibliography '("~/pdfs/bibfile.bib"))
    ;;(citar-open-entry-function #'citar-open-entry-in-zotero)
    (citar-open-entry-function #'citar-open-entry-in-file)
    (citar-library-paths '("~/pdfs" "~/pdfs/books"))
    :hook
    (LaTeX-mode . citar-capf-setup)
    (org-mode . citar-capf-setup))
  (setq org-cite-global-bibliography '("~/pdfs/bibfile.bib"))

  (use-package citar-embark
    :after citar embark
    :no-require
    :config (citar-embark-mode))

  (use-package citar-org-roam
    :after (citar org-roam)
    :config (citar-org-roam-mode))
  (setq citar-org-roam-note-title-template "${author} - ${title}")
  (setq org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           :target
           (file+head
            "%<%Y%m%d%H%M%S>-${slug}.org"
            "#+title: ${note-title}\n#+STARTUP: latexpreview")
           :unnarrowed t)
          ("n" "literature note" plain
           "%?"
           :target
           (file+head
            "%(expand-file-name (or citar-org-roam-subdir \"\") org-roam-directory)/${citar-citekey}.org"
            "#+title: ${citar-citekey} (${citar-date}). ${note-title}.\n#+created: %U\n#+last_modified: %U\n\n#+STARTUP: latexpreview")
           :unnarrowed t)))
  (setq citar-org-roam-capture-template-key "n")

  (use-package org-ref
    :after (org-roam org)
    :config
    (setq org-ref-default-bibliography '("~/pdfs/bibfile.bib")
          org-ref-pdf-directory "~/pdfs/"))
  (require 'doi-utils)

  (use-package bibtex-completion
    :after (org-roam org)
    :custom
    (bibtex-completion-bibliography '("~/pdfs/bibfile.bib"))
    (bibtex-completion-library-path '("~/pdfs"))
    (bibtex-completion-notes-path '("~/org/roam")))

  ;; Sci-hub
(defun sci-hub-pdf-url (doi)
  "Get url to the pdf from SCI-HUB"
  (setq *doi-utils-pdf-url* (concat "https://sci-hub.se/" doi) ;captcha
        *doi-utils-waiting* t
        )
  ;; try to find PDF url (if it exists)
  (url-retrieve (concat "https://sci-hub.se/" doi)
            (lambda (status)
              (goto-char (point-min))
              (while (search-forward-regexp "\\(https://\\|//sci-hub.se/downloads\\).+download=true'" nil t)
                (let ((foundurl (match-string 0)))
                  (message foundurl)
                  (if (string-match "https:" foundurl)
                  (setq *doi-utils-pdf-url* foundurl)
                (setq *doi-utils-pdf-url* (concat "https:" foundurl))))
                (setq *doi-utils-waiting* nil))))
  (while *doi-utils-waiting* (sleep-for 0.1))
  *doi-utils-pdf-url*)

(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0)
  (which-key-mode))

(use-package helpful
  ;;:custom
  ;;(counsel-describe-function-function #'helpful-callable)
  ;;(counsel-describe-variable-function #'helpful-variable)
  :bind
  ;;([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ;;([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package magit)
(use-package transient) ;; for magit
(use-package vterm)
(use-package pdf-tools
  :config
  (pdf-tools-install :no-query)
  (require 'pdf-info))
(defun my-pdf-view-mode-hook ()
  "Custom hook to fit PDF page to window on opening"
  (pdf-view-fit-page-to-window)
  (auto-revert-mode))
(add-hook 'pdf-view-mode-hook 'my-pdf-view-mode-hook)
;;(add-hook 'pdf-view-mode-hook (lambda () (pdf-view-midnight-minor-mode)))

(use-package org-download
  :config
  (setq org-download-image-dir "~/Figures/")  ; Set the directory where images will be saved
  (setq org-download-screenshot-method "gnome-screenshot -a -f %s")  ; Set the method for screenshot
  (add-hook 'dired-mode-hook 'org-download-enable) ;Enable org-download in dired-mode
  (org-download-enable))  

(use-package org-mac-image-paste
  :straight (org-mac-image-paste :type git :host github :repo "jdtsmith/org-mac-image-paste")
  :if (eq window-system 'mac)
  :config
  (org-mac-image-paste-mode 1)
  (setq org-use-property-inheritance t) ;Inherit :ID/etc. from parent nodes
  (setq org-image-actual-width nil)  ;allow #+ATTR_ORG: :width 300 etc. 
  (setq org-attach-id-dir "../../Figures") ;; copy-pasted files in Figures dir
  ;; (setq org-attach-id-dir ".org-attach") ; make the attachment directory less visible

  ;; Optional: You can bind the paste image function to a key if desired
  (define-key org-mode-map (kbd "C-c C-x p") #'org-mac-image-paste)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(file-name-nondirectory (expand-file-name "~/references/images"))

(defun my-org-paste-image-to-dir ()
  "Paste an image into a time stamped unique-named file in the
  same directory as the org-buffer and insert a link to this file."
  (interactive)
  (let* ((image-filename
           (concat
            (read-from-minibuffer "Enter something: ") ".png"))
	  (unix-path
           (concat
              (expand-file-name "~")
              "/references/images/"
              image-filename))
          (wsl-path
           (as-windows-path (concat
              (expand-file-name "~")
              "/references/images/"
              image-filename)))
          (ps-script
           (concat "(Get-Clipboard -Format image).Save('" wsl-path "')")))

    (powershell ps-script)
    (message "here is") (message wsl-path) (message image-filename)

    (if (file-exists-p unix-path)
        (progn (insert (concat "[[" unix-path"]]"))
               (org-display-inline-images))
      (user-error
       "Error pasting the image, make sure you have an image in the clipboard!"))
    ))

(file-exists-p "\\wsl.localhost\\arch\\home\\angelo\\references\\images\\test.png")
(defun my-org-paste-image ()
  "Paste an image into a time stamped unique-named file in the
  same directory as the org-buffer and insert a link to this file."
  (interactive)
  (let* ((target-file
          (concat
           (make-temp-name
            (concat (buffer-file-name)
                    "_"
                    (format-time-string "%Y%m%d_"))) ".png"))
         (wsl-path
          (concat (as-windows-path(file-name-directory target-file))
                  "\\"
                  (file-name-nondirectory target-file)))
         (ps-script
          (concat "(Get-Clipboard -Format image).Save('" wsl-path "')")))

    (powershell ps-script)

    (if (file-exists-p target-file)
        (progn (insert (concat "[[" target-file "]]"))
               (org-display-inline-images))
      (user-error
       "Error pasting the image, make sure you have an image in the clipboard!"))
    ))

(defun as-windows-path (unix-path)
  "Takes a unix path and returns a matching WSL path
  (e.g. \\\\wsl$\\Ubuntu-20.04\\tmp)"
  ;; substring removes the trailing \n
  (substring
   (shell-command-to-string
    (concat "wslpath -w " unix-path)) 0 -1))

(defun powershell (script)
  "executes the given script within a powershell and returns its return value"
  (call-process "powershell.exe" nil nil nil
                "-Command" (concat "& {" script "}")))

(use-package general
  :config
  (general-create-definer am/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (general-define-key ;; evil overrides
   :states '(normal visual)
   :keymaps 'global-map
   "C-f" 'consult-ripgrep
   "C-." 'embark-act
   "C-i" 'evil-jump-forward)
  (am/leader-keys
    "b"  '(:ignore b :which-key "Buffer")
    "bb" '(next-buffer :which-key "Next")
    "bn" '(next-buffer :which-key "Next")
    "bp" '(previous-buffer :which-key "Previous")
    "bN" '(previous-buffer :which-key "Previous")
    "bl" '(consult-buffer :which-key "Switch")
    "bk" '(kill-buffer :which-key Kill)

    "w"  '(:ignore w :which-key "Window")
    "ww" '(evil-window-next :which-key "Next")
    "wn" '(evil-window-next :which-key "Next")
    "wN" '(evil-window-prev :which-key "Previous")
    "ws" '(evil-window-split :which-key "Horizontal Split")
    "wv" '(evil-window-vsplit :which-key "Vertical Split")
    "wc" '(evil-window-delete :which-key "Close")

    ":" '(execute-extended-command :which-key "M-x")

    "h"  '(:ignore h :which-key "Help")
    "hv" '(describe-variable :which-key "Describe Variable")
    "hf" '(describe-function :which-key "Describe Function")
    "hi" '(indent-region :which-key "Indent Region")
    "hs" '(describe-symbol :which-key "Describe Symbol")
    "hm" '(describe-mode :which-key "Describe Mode")
    "hk" '(describe-key :which-key "Describe Key")

    "f"  '(:ignore f :which-key "Files")
    "fr" '(consult-recent-file :which-key "Recent Files")
    "ff" '(find-file :which-key "Find File")

    "l"  '(:ignore l :which-key "Latex")
    "lg" '(pdf-sync-forward-search :which-key "source-to-pdf")

    "o"  '(:ignore o :which-key "org")
    "ot" '(org-babel-tangle :which-key "Tangle")
    "of" '(org-roam-node-find :which-key "Find Node")
    "od" '(org-toggle-inline-images :which-key "Toggle Images")
    "or" '(org-mac-image-paste-refresh-this-node :which-key "Refresh Images")
    "oe" '(org-edit-special :which-key "org Edit Special")

    "c"  '(:ignore c :which-key "Citations")
    "ci" '(citar-insert-citation :which-key "insert-citation")
    "ce" '(citar-open-entry :which-key "open-entry")
    ))

;; (use-package lsp-mode
;; :init
;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;; (setq lsp-keymap-prefix "C-c l")
;; :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;; (python-mode . lsp-deferred)
;; if you want which-key integration
;; (lsp-mode . lsp-enable-which-key-integration))
;; :commands lsp lsp-deferred)

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (setq lsp-keymap-prefix "C-c l")
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook (
         (lsp-completion-mode . my/lsp-mode-setup-completion)
         (python-mode . lsp-deferred)
         (LaTeX-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp lsp-deferred)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))  ; or lsp-deferred

(use-package lsp-latex
  ;; this uses texlab
  :ensure t
  :config
  (progn
    (add-hook 'bibtex-mode-hook 'lsp)
    )
  )
(use-package lsp-ui :commands lsp-ui-mode)

(use-package micromamba
  :init
  (when (eq system-type 'gnu/linux) ;; Check if the OS is gnu/linux
  (setq micromamba-executable "~/.local/bin/micromamba"))
  :config
  (micromamba-activate "general"))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(add-hook 'python-ts-mode-hook #'run-python)

(setq python-indent-offset 4)
(setq org-edit-src-content-indentation 2)
(setq org-src-tab-acts-natively t)
(setq evil-auto-indent t)
