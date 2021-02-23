(setq inhibit-startup-message t)

(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1)   ; Disable the toolbar
(tooltip-mode -1)    ; Disable tooltips
(set-fringe-mode 10) ; 

(menu-bar-mode -1)   ; Disable the menu bar

(setq visible-bell t); Set up the visible bell

;; Set Fonts
(set-face-attribute 'default nil :font "Fira Code Retina" :height 100)

(load-theme 'wombat) ; Load Theme

;; Initialize package sources

(require 'package)

(setq package-archives '(("melpa". "https://melpa.org/packages/")
			 ("org"  . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package command-log-mode)


(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line) 
         ("C-k" . ivy-previuos-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previos-line)
         ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previos-line)
	 ("C-d" . ivy-reverse-i-search-kill))        
  :config
  (ivy-mode 1))


;; Dooms Mode Line
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((dooms-modeline-height 10)))

