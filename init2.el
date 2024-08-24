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
  		eshell-mode-hook
  		pdf-view-mode-hook))
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
;; (use-package ivy-bibtex
;;   :after (ivy)
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

(use-package auctex
  ;;:defer t
  :hook ((LaTeX-mode . LaTeX-preview-setup)
         (LaTeX-mode . turn-on-reftex)   ;; Enable RefTeX for cross-referencing
         ;;(LaTeX-mode . flyspell-mode)    ;; Enable Flyspell for spell checking
         (LaTeX-mode . LaTeX-math-mode)) ;; Enable LaTeX Math mode
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)         ;; Ask for master file when opening a new TeX file
  (setq TeX-PDF-mode t))

(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
      LaTeX-command-style '(("" "%(PDF)%(latex) --synctex=1 %(file-line-error) %(extraopts) %(output-dir) %S%(PDFout)"))
      TeX-source-correlate-start-server t) ;; not sure if last line is neccessary
;; to use pdfview with auctex

(use-package company-auctex
  :after (company auctex)
  :config
  (company-auctex-init))

(use-package latex-preview-pane
  ;;:after auctex
  :hook (LaTeX-mode . latex-preview-pane-mode))

;; Key bindings and documentation string
;; (use-package ivy-rich
;;   :config
;;   (ivy-rich-mode 1))

(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0)
  (which-key-mode))

;; (use-package counsel
;;   :bind (("M-x" . counsel-M-x)
;;          ("C-x b" . counsel-ibuffer)
;;          ("C-x C-f" . counsel-find-file)
;;          :map minibuffer-local-map
;;          ("C-r" . 'counsel-minibuffer-history))
;;   :config
;;   (setq ivy-initial-inputs-alist nil))

;; (use-package helpful
;;   :custom
;;   (counsel-describe-function-function #'helpful-callable)
;;   (counsel-describe-variable-function #'helpful-variable)
;;   :bind
;;   ([remap describe-function] . counsel-describe-function)
;;   ([remap describe-command] . helpful-command)
;;   ([remap describe-variable] . counsel-describe-variable)
;;   ([remap describe-key] . helpful-key))

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

(use-package pdf-tools
  :config
  (pdf-tools-install :no-query))

(defun my-pdf-view-mode-hook ()
  "Custom hook to fit PDF page to window on opening"
  (pdf-view-fit-page-to-window))
(add-hook 'pdf-view-mode-hook 'my-pdf-view-mode-hook)
(add-hook 'pdf-view-mode-hook (lambda () (pdf-view-midnight-minor-mode)))

(use-package citar
  :custom
  (citar-bibliography '("~/org/references/bibfile.bib"))
  (citar-open-entry-function #'citar-open-entry-in-zotero)
  ;;(citar-library-paths '("~/Zotero/storage*"))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))
(setq org-cite-global-bibliography '("~/org/references/bibfile.bib"))


(use-package marginalia
  :config
  (marginalia-mode))

  (use-package embark
    :bind
    (("C-." . embark-act)         ;; pick some comfortable binding
     ("C-;" . embark-dwim)        ;; good alternative: M-.
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
;;   ;; Consult users will also want the embark-consult package.
;;   (use-package embark-consult
;;     :hook
;;     (embark-collect-mode . consult-preview-at-point-mode))

;; Example configuration for Consult
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
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
n)


(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode))

(use-package org-download
  :config
  (setq org-download-image-dir "~/Figures/")  ; Set the directory where images will be saved
  (setq org-download-screenshot-method "gnome-screenshot -a -f %s")  ; Set the method for screenshot
  (add-hook 'dired-mode-hook 'org-download-enable) ;Enable org-download in dired-mode
  (org-download-enable))

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

(use-package general
  :config
  (general-create-definer am/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (am/leader-keys
    "t"  '(:ignore t :which-key "Toggles")
    ;;    "tt" '(counsel-load-theme :which-key "Choose Theme")

    "b"  '(:ignore b :which-key "Buffer")
    "bb" '(next-buffer :which-key "Next")
    "bn" '(next-buffer :which-key "Next")
    "bp" '(previous-buffer :which-key "Previous")
    "bN" '(previous-buffer :which-key "Previous")
    ;;    "bl" '(counsel-switch-buffer :which-key "Switch")
    "bk" '(kill-buffer :which-key Kill)

    "w"  '(:ignore w :which-key "Window")
    "ww" '(evil-window-next :which-key "Next")
    "wn" '(evil-window-next :which-key "Next")
    "wN" '(evil-window-prev :which-key "Previous")
    "ws" '(evil-window-split :which-key "Horizontal Split")
    "wv" '(evil-window-vsplit :which-key "Vertical Split")
    "wc" '(evil-window-delete :which-key "Close")

    ;;    ":" '(counsel-M-x :which-key "M-x")

    "h"  '(:ignore h :which-key "Help")
    ;;    "hv" '(counsel-describe-variable :which-key "Describe Variable")
    ;;    "hf" '(counsel-describe-function :which-key "Describe Function")
    "hi" '(indent-region :which-key "Indent Region")

    "f"  '(:ignore f :which-key "Files")
    ;;    "fr" '(counsel-recentf :while-key "Recent Files")
    "ff" '(find-file :while-key "Find File")

    "l"  '(:ignore l :which-key "Latex")
    "lg" '(pdf-sync-forward-search :which-key "source-to-pdf")
    "lc" '(citar-insert-citation :which-key "insert-citation")
    "le" '(citar-open-entry :which-key "open-entry")

    "e"  '(:ignore e :which-key "Embark")
    "ea" '(embark-act :which-key "act")
    "ed" '(embark-dwim :which-key "dwim")

    "o"  '(:ignore c :which-key "org")
    "oc" '(citar-insert-citation :which-key "insert-citation")
    "oe" '(citar-open-entry :which-key "open-entry")
    "ot" '(org-babel-tangle :which-key "tangle")
    ))
;; Enable vertico
(use-package vertico
  ;; :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "C-j") 'vertico-next)
  (define-key vertico-map (kbd "C-k") 'vertico-previous))

(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :custom
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

(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

