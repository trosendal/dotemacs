(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(split-window-right)
(dired ".")
(text-scale-set -2)

;; Get rid of splash screen

(setq inhibit-splash-screen t)

;; Don't use messages that you don't read
(setq initial-scratch-message "")

;; Backup file location

(if (file-exists-p "~/.saves")
    nil
  (make-directory "~/.saves"))
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; R startup
(setq inferior-R-args "--no-restore-history --no-save")

(global-set-key (kbd "C-c r") 'R)
(global-set-key (kbd "C-c s") 'shell)

;(setq ess-tab-complete-in-script t)

(add-to-list 'load-path "~/.emacs.d/auto-complete/")
(add-to-list 'load-path "~/.emacs.d/popup/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories ".emacs.d/auto-complete/dict")
(ac-config-default)

;;Latex compilation and preview

;(add-to-list 'load-path "~/.emacs.d/auctex/")
;(require 'tex-site)


;(load "auctex.el")
;(load "preview-latex.el")

;(setq TeX-auto-save t)
;(setq TeX-parse-self t)
;(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;(setq reftex-plug-into-AUCTeX t)

;(setq TeX-PDF-mode t)

;Add flyspell to markdown mode

(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'flyspell-buffer)


; Add a feature to copy a line from anywhere in the line

(defun my/copy-line (x)			; define a function with the name and parameters
  "Copy the current line"		; Comment
  (interactive "p")			; make the function interactive of the type that takes a parameter
  (kill-ring-save			; execute the kill-ring-save function
   (line-beginning-position)		; first argument of 'kill-ring-save'
   (line-beginning-position(+ 1 x))	; second argument of 'kill-ring-save' (The position of the next line start point)
  )
)


(global-set-key (kbd "C-j") 'my/copy-line)


; Allow a to be used in dired mode


(put 'dired-find-alternate-file 'disabled nil)
