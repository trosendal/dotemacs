(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;;Dictionary
(setq ispell-dictionary "british")

(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word)
  )
(global-set-key (kbd "<f8>") 'flyspell-check-next-highlighted-word)

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, 
ask for installation if it’s not.
Return a list of installed packages
or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages)
  )
(package-initialize)
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(ensure-package-installed 'magit 'ess 'auto-complete 'popup 'auctex 'markdown-mode 'polymode 'multi-web-mode)
(package-initialize)

;;Org-mode
(add-to-list 'load-path "/usr/share/emacs/site-lisp/org/")
(setq org-todo-keyword-faces
      '(
        ("TODO" . (:foreground "#FF0000" :weight bold))
        ("UPDATE" . (:foreground "#E61A00" :weight bold))
        ("SUBMITTED" . (:foreground "#CC3300" :weight bold))
	("CONVERTED" . (:foreground "#B34D00" :weight bold))
	("TYPESET" . (:foreground "#996600" :weight bold))
	("PROOF" . (:foreground "#808000" :weight bold))
	("AUTHOR-FEEDBACK" . (:foreground "#996600" :weight bold))
	("AUTHOR-OK" . (:foreground "#4DB300" :weight bold))
	("REVIEW" . (:foreground "#33CC00" :weight bold))
	("EDITOR" . (:foreground "#1AE600" :weight bold))
	("DONE" . (:foreground "#00FF00" :weight bold))
        ))

;; ;; Get rid of splash screen

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

;; shortcuts
(global-set-key (kbd "C-c r") 'R)
(global-set-key (kbd "C-c s") 'shell)
(global-set-key (kbd "C-c m") 'compile)

(setq ess-tab-complete-in-script t)

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories ".emacs.d/auto-complete/dict")
(ac-config-default)

;; ;;Latex compilation and preview

(require 'tex-site)

;; (load "preview-latex.el")

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-buffer)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(setq LaTeX-item-indent 2)
(setq LaTeX-indent-level 2)
(setq TeX-brace-indent-level 2)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; ;Add flyspell to markdown mode

(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'flyspell-buffer)
(add-hook 'markdown-mode-hook 'auto-fill-mode)

;; ; Allow a to be used in dired mode

(put 'dired-find-alternate-file 'disabled nil)

;;encoding
(prefer-coding-system 'utf-8)
;(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;magit keybindings
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-last-seen-setup-instructions "1.4.0")

(global-set-key (kbd "C-x C-b") 'ibuffer)
(define-key help-map (kbd "7") 'xah-lookup-google)
(define-key help-map (kbd "8") 'xah-lookup-wikipedia)

(setq delete-by-moving-to-trash t)

;resize windows quickly
(global-set-key (kbd "C-M-6") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-7") 'shrink-window-horizontally)

(defun transpose-windows (arg)
   "Transpose the buffers shown in two windows."
   (interactive "p")
   (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
     (while (/= arg 0)
       (let ((this-win (window-buffer))
             (next-win (window-buffer (funcall selector))))
         (set-window-buffer (selected-window) next-win)
         (set-window-buffer (funcall selector) this-win)
         (select-window (funcall selector)))
       (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(global-set-key (kbd "C-x t") 'transpose-windows)

;; ; Polymode for rmd files
(require 'poly-R)
(require 'poly-markdown)
(require 'ess-site)
(autoload 'r-mode "ess-site" "(Autoload)" t)
;;; MARKDOWN
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))

;;; R modes
(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

;; ;;; Fixed the dead tilda in ubuntu
(require 'iso-transl)

;; ;cycle forward in the kill ring

(defun yank-pop-forwards (arg)
      (interactive "p")
      (yank-pop (- arg)))

(global-set-key "\M-Y" 'yank-pop-forwards)

;; ;set a theme

(load-theme 'whiteboard)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((latex . t)))

;; ;;function to custom open pdfs with evince or other files like gnome
;; ;;does when you double click

(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "gnome-open" nil 0 nil file)
    (message "Opening %s done" file)))

;; ;;open with foxit from dired
;; (defun foxit()
;;   "In dired, open the file with foxit"
;;   (interactive)
;;   (let* ((file (dired-get-filename nil t)))
;;     (message "Opening %s..." file)
;;     (call-process "~/opt/foxitsoftware/foxitreader/FoxitReader" nil 0 nil file)
;;     (message "Opening %s done" file)))

;; ;;html and javascript editing

;; (add-to-list 'load-path "~/.emacs.d/elpa/multi-web-mode-0.1/")
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags 
  '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
    (js-mode  "<script[^>]*>" "</script>")
    (css-mode "<style[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)
