;; Initial setup
(setq inhibit-startup-message t)

(scroll-bar-mode -1)  ; Disable visible toolbar
(tool-bar-mode -1)    ; Disable toolbar (new/open/save file buttons etc.)
(menu-bar-mode -1)    ; Disable menu bar
(tooltip-mode -1)     ; Disable tooltips (?)
(set-fringe-mode 5)   ; Margins from left and right

(set-face-attribute 'default nil :height 110) ; Set the default font

(load-theme 'tango-dark)		; Theme

(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; Escape key to quit

;; Remove the annoying freeze on Ctrl-z
(global-unset-key (kbd "C-z"))

;; Global line and column number mode
(column-number-mode)
(global-display-line-numbers-mode t)

;; Enable recursive minibuffers
(set 'enable-recursive-minibuffers t)

;; Terminal
(use-package multi-term
  :ensure t
  :bind (([f1] . multi-term)
         :map term-raw-map
         ("C-c C-j" . term-line-mode)
         :map term-mode-map
         ("C-c C-k" . term-char-mode))
  :config
  ;; (setq multi-term-dedicated-select-after-open-p t
  ;;       multi-term-dedicated-window-height 25
  ;;       multi-term-program "/bin/bash")

  ;; ;; Enable compilation-shell-minor-mode in multi term.
  ;; ;; http://www.masteringemacs.org/articles/2012/05/29/compiling-running-scripts-emacs/

  ;; ;; TODO: WTF? Turns off colors in terminal.
  ;; ;; (add-hook 'term-mode-hook 'compilation-shell-minor-mode)
  ;; (add-hook 'term-mode-hook
  ;;           (lambda ()
  ;;             (dolist
  ;;                 (bind '(
  ;;                         ;; ("<S-down>" . multi-term)
  ;;                         ;; ("<S-left>" . multi-term-prev)
  ;;                         ;; ("<S-right>" . multi-term-next)
  ;;                         ;; ("C-<backspace>" . term-send-backward-kill-word)
  ;;                         ;; ("C-<delete>" . term-send-forward-kill-word)
  ;;                         ;; ("C-<left>" . term-send-backward-word)
  ;;                         ;; ("C-<right>" . term-send-forward-word)
  ;;                         ("C-c C-j" . term-line-mode)
  ;;                         ("C-c C-k" . term-char-mode)
  ;;                         ;; ("C-v" . scroll-up)
  ;;                         ;; ("C-y" . term-paste)
  ;;                         ;; ("C-z" . term-stop-subjob)
  ;;                         ;; ("M-DEL" . term-send-backward-kill-word)
  ;;                         ;; ("M-d" . term-send-forward-kill-word)))
  ;;                         ))
  ;;               (add-to-list 'term-bind-key-alist bind))))
  ;; (add-hook 'term-mode-hook (lambda ()
  ;;                             (define-key term-mode-map (kbd "C-c C-j") 'term-line-mode)
  ;;                             (define-key term-mode-map (kbd "C-c C-k") 'term-char-mode)
  ;;                             ))
  )


(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
	 (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			                   ("org" . "https://orgmode.org/elpa/")
			                   ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)			; Initialise the package archives above
(unless package-archive-contents
  (package-refresh-contents))	        ; Useful for the very first run
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))	; Install use-package
(require 'use-package)
(setq use-package-always-ensure t)     ; Download and install the packages before they are use-package'd

;; Try a package without installing
(use-package try)

;; Navigate between windows
(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))
    ))

;; Ivy for the smart completion system
(use-package counsel			;; counsel is used by ivy-rich
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))
  ;; :config
  ;; (setq ivy-initial-inputs-alist nil))	; don't start searches with ^

(use-package diminish)			; Don't show the full name of the mode
(use-package swiper)			; Neat search package
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("RET" . ivy-alt-done))
  :config
  (ivy-mode 1)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  (setq ivy-use-virtual-buffers t))

(use-package ivy-rich
  :init (ivy-rich-mode 1))
;; (require 'ivy-rich)
;; (ivy-rich-mode 1)
;; (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

;; Doom modeline for the mode and status bar
(require 'all-the-icons)		; Required for the doom-modeline
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-city-lights t)
  (set-face-foreground 'font-lock-doc-face "SpringGreen3")
  (set-face-foreground 'font-lock-comment-face "SeaGreen4")
  (set-face-foreground 'font-lock-function-name-face "yellow3")
  (set-face-background 'default "gray10")
  ;; Enable flashing mode-line on errors
  ; (doom-themes-visual-bell-config)
  
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config (setq doom-modeline-height 25))


;; Rainbow delimiters (easy to read parentheses)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;; Which-key mode
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; Helpful for better help screens
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


;; Custom key bindings

(fset 'down-one-line "\C-u1\C-v")
(fset 'up-one-line "\C-u1\366")
(global-set-key (kbd "M-<down>") 'down-one-line) ; Alt-Up for one line up
(global-set-key (kbd "M-<up>") 'up-one-line)	 ; Alt-Down for one line down

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(js-indent-level 2)
;;  '(package-selected-packages
;;    '(try org-bullets dumb-jump counsel-projectile projectile add-node-modules-path flycheck web-mode xref-js2 js2-refactor js2-mode unicode-fonts helpful counsel ivy-rich which-key rainbow-delimiters doom-themes doom-modeline diminish swiper ivy use-package))
;;  '(web-mode-code-indent-offset 2)
;;  '(web-mode-markup-indent-offset 2)
;;  '(winner-mode t))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )

;; Unicode support
;; (use-package unicode-fonts
;;   :config
;;   (unicode-fonts-setup))

(use-package magit
  :bind (("C-x g" . magit-status)))


;; Python: autodetect the indentation style
(use-package python
  :config
  (setq python-shell-interpreter "/bin/python3"))

(defun how-many-region (begin end regexp &optional interactive)
  "Print number of non-trivial matches for REGEXP in region.                    
   Non-interactive arguments are Begin End Regexp"
  (interactive "r\nsHow many matches for (regexp): \np")
  (let ((count 0) opoint)
    (save-excursion
      (setq end (or end (point-max)))
      (goto-char (or begin (point)))
      (while (and (< (setq opoint (point)) end)
                  (re-search-forward regexp end t))
        (if (= opoint (point))
            (forward-char 1)
          (setq count (1+ count))))
      (if interactive (message "%d occurrences" count))
      count)))

(defun infer-indentation-style ()
  "if our source file uses tabs, we use tabs, if spaces spaces, and if neither, we use the current 'indent-tabs-mode'" 
  (let ((space-count (how-many-region (point-min) (point-max) "^  "))
        (tab-count (how-many-region (point-min) (point-max) "^\t")))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

(setq indent-tabs-mode nil)
(infer-indentation-style)

(add-hook 'python-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)
	    (infer-indentation-style)))


;; JS-2
(use-package js2-mode
  :config
  ;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode))
(use-package js2-refactor
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-r")
 )
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)
(use-package xref-js2
  :config
  (add-hook 'js2-mode-hook
            (lambda () (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
 )
;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

;; React (web-mode)
(use-package web-mode
  :custom
  (web-mode-attr-indent-offset nil)
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-sql-indent-offset 2)
  (indent-tabs-mode nil)
  (tab-width 2)
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode)) ;; auto-enable for .js/.jsx files
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  )

;; (defun web-mode-init-hook ()
;;   "Hooks for Web mode.  Adjust indent."
;;   (setq web-mode-markup-indent-offset 2)
;;   (setq web-mode-code-indent-offset 2))
;; (add-hook 'web-mode-hook 'web-mode-init-hook)

;; Flycheck
(use-package flycheck
  :config
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(javascript-jshint json-jsonlist)))
  ;; Enable eslint checker for web-mode
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  ;; Enable flycheck globally
  (add-hook 'after-init-hook #'global-flycheck-mode)
  )

;; Enable eslint checker when web-mode is activated:
(use-package add-node-modules-path
  :config
  (add-hook 'flycheck-mode-hook 'add-node-modules-path)
 )

;; projectile for project management
(use-package projectile
:ensure t
:config
(projectile-mode)
(setq projectile-completion-system 'ivy)
:bind (:map projectile-mode-map
            ("M-p" . projectile-command-map)))

(use-package counsel-projectile
:ensure t
:config
(counsel-projectile-mode))

;; Org mode
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )

;; (use-package dumb-jump
;; ;; :bind (("M-g o" . dumb-jump-go-other-window)
;; ;; ("M-g j" . xref-find-definitions)
;; ;; ("M-g x" . dumb-jump-go-prefer-external)
;; ;; ("M-g z" . dumb-jump-go-prefer-external-other-window))
;; :hook ((prog-mode . dumb-jump-mode))
;; :config (setq dumb-jump-selector 'ivy)
;; :init
;; (dumb-jump-mode)
;; )
(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-default-project "~/Corpy/Confide/confide-frontend"))

;; Company
(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode))
;; (require 'company-web-html)
(use-package org)
;; (org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))

;; Backups
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq backup-by-copying t) ;; slowest but safest
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(multi-term xref-js2 which-key web-mode use-package unicode-fonts try rainbow-delimiters python-mode org-bullets magit js2-refactor ivy-rich helpful flycheck dumb-jump doom-themes doom-modeline diminish counsel-projectile company add-node-modules-path ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
