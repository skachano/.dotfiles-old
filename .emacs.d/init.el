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


;; Global line and column number mode
(column-number-mode)
(global-display-line-numbers-mode t)

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
  (package-install 'use-package))	; Install use-package
(require 'use-package)
(setq use-package-always-ensure t)     ; Download and install the packages before they are use-package'd 


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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(unicode-fonts helpful counsel ivy-rich which-key rainbow-delimiters doom-themes doom-modeline diminish swiper ivy use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Unicode support
;; (use-package unicode-fonts
;;   :config
;;   (unicode-fonts-setup))

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
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if        
  ;; neither, we use the current indent-tabs-mode                               
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


;; Backups
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq backup-by-copying t) ;; slowest but safest
