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

(split-window-right)
(dired "/media/trosendal/OS/projects")
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

;;BUGS
(add-to-list 'load-path "~/.emacs.d/elpa/ess-20151107.1531/lisp/")
(require 'ess-jags-d)

;; shortcuts
(global-set-key (kbd "C-c r") 'R)
(global-set-key (kbd "C-c s") 'shell)
(global-set-key (kbd "C-c m") 'compile)

;(setq ess-tab-complete-in-script t)

(add-to-list 'load-path "~/.emacs.d/auto-complete/")
(add-to-list 'load-path "~/.emacs.d/popup/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories ".emacs.d/auto-complete/dict")
(ac-config-default)

;;Latex compilation and preview

;(require 'tex-site)

;(load "preview-latex.el")

;(setq TeX-auto-save t)
;(setq TeX-parse-self t)
;(setq-default TeX-master nil)

(add-to-list 'load-path "~/.emacs.d/elpa/auctex-11.89.1/")
(load "auctex.el" nil t t)
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

;(setq reftex-plug-into-AUCTeX t)

;(setq TeX-PDF-mode t)

;Add flyspell to markdown mode

(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'flyspell-buffer)
(add-hook 'markdown-mode-hook 'auto-fill-mode)

; Add a feature to copy a line from anywhere in the line

(defun tr/copy-line (x)			; define a function with the name and parameters
  "Copy the current line"		; Documentation
  (interactive "p")			; make the function interactive of the type that takes a parameter
  (kill-ring-save			; execute the kill-ring-save function
   (line-beginning-position)		; first argument of 'kill-ring-save'
   (line-beginning-position(+ 1 x))	; second argument of 'kill-ring-save' (The position of the next line start point)
  )
)

(global-set-key (kbd "C-j") 'tr/copy-line)

(defun tr/add-numeric ()
  "Function to replace numbers with latex numprint formatted numbers "
  (interactive)
  (goto-char (point-min))
  (goto-char (search-forward-regexp "section"))
  ;; Find and replace numbers with 5 or more digits followed by
  ;; optional decimal and any number of decimal digits
  (while (search-forward-regexp "\\(-?\\)\\([0-9]\\{5,\\}\\([.][0-9]+\\)?\\)" nil t)
    (replace-match "\\1\\\\numprint{\\2}")
    )
  ;; Go back to start
  (goto-char (point-min))
  (goto-char (search-forward-regexp "section"))
  (while (search-forward-regexp "[^\{]\\(-?\\)\\([0-9]+[.][0-9]+\\)" nil t)
     (replace-match "\\1\\\\numprint{\\2}")
     )
  )


(defun tr/test ()
  "Function to replace numbers with latex numprint formatted numbers "
  (interactive)
  (save-excursion
  (goto-char (point-min))
  (goto-char (search-forward-regexp "section"))
  (search-forward-regexp "\\([]\\)\\([0-9]+[.]?[0-9]*\\)" nil t)
    (let(start)
    (setq start (match-beginning 0))
    (insert "}")
    (goto-char start)
    (insert "numprint{"))))

;; Check for number gaps
;; query-replace-regexp
;; \([0-9]\)\s-\([0-9]\)
;; \1\2

;; Check for numbers with commas
;; query-replace-regexp
;; \([0-9]\),\([0-9]\)
;; \1\2

;; Check for numbers with tildas
;; query-replace-regexp
;; \([0-9]\)~\([0-9]\)
;; \1\2

;; Replace ä
;; query-replace-regexp
;; ä
;; {\"a}

;; Replace å
;; query-replace-regexp
;; å
;; {\aa}

;; Replace ö
;; query-replace-regexp
;; ö
;; {\"o}

; Allow a to be used in dired mode

(put 'dired-find-alternate-file 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;;  '(csv-separators (quote (";" "	")))
;;  '(custom-safe-themes (quote ("6a17a056a51cc680e0011a67f6c7424bc47d34fd1fc294ba093531deb3de5b68" default)))
'(send-mail-function (quote smtpmail-send-it))
'(smtpmail-smtp-server "smtp1.sva.se")
'(smtpmail-smtp-service 25))
;; (custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; )

;Email address
(setq user-mail-address "thomas.rosendal@sva.se")
(setq user-full-name "Thomas Rosendal")

;; gnus setup
(require 'gnus)
(setq nnml-directory "~/mail")
(setq message-directory "~/mail")
(setq gnus-select-method
      '(nnimap "outlook.office365.com"
               (nnimap-address "outlook.office365.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)))
 (setq gnus-thread-sort-functions
       '(gnus-thread-sort-by-number
       (not gnus-thread-sort-by-date)))
;;encoding
(prefer-coding-system 'utf-8)
;(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;magit keybindings
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-last-seen-setup-instructions "1.4.0")

;; ;http://stackoverflow.com/questions/8334263/in-emacs-how-to-replace-only-on-matching-lines
;; (defun my-replace-on-matching-lines (&optional arg)
;;   "Replace text on lines that match a regexp.
;; With prefix arg, replace on non-matching lines."
;;   (interactive "P")
;;   (let* ((regexp (concat ".*"
;;                          (read-from-minibuffer
;;                           (concat "Replace on lines "
;;                                   (if arg "not " "")
;;                                   "matching regexp: "))))
;;          (replace (read-from-minibuffer "Replace: "))
;;          (with (read-from-minibuffer (concat "Replace " replace " with: ")))
;;          match)
;;     (save-excursion
;;       (goto-char (point-min))
;;       (while (not (eobp))
;;         (setq match (looking-at regexp))
;;         (when (if arg (not match) match)
;;           (while (search-forward replace (point-at-eol) t)
;;             (replace-match with nil t)))
;; 	(add-text-properties (match-beginning 1)
;; 			     (match-end 1)
;; 			     (list 'face 'bold))
;;         (forward-line)))))

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

; Polymode for rmd files
(add-to-list 'load-path "~/.emacs.d/polymode/")
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

;;; Fixed the dead tilda in ubuntu
(require 'iso-transl)

;cycle forward in the kill ring

(defun yank-pop-forwards (arg)
      (interactive "p")
      (yank-pop (- arg)))

(global-set-key "\M-Y" 'yank-pop-forwards) ; M-Y (Meta-Shift-Y)

;set a theme

(load-theme 'whiteboard)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
;;(setq exec-path (append exec-path '("/usr/bin")))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((latex . t)))

(load-file "~/.emacs.d/lisp/svar/svar.el")

;;function to custom open pdfs with evince or other files like gnome
;;does when you double click

(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "gnome-open" nil 0 nil file)
    (message "Opening %s done" file)))

;;open with foxit from dired
(defun foxit()
  "In dired, open the file with foxit"
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "~/opt/foxitsoftware/foxitreader/FoxitReader" nil 0 nil file)
    (message "Opening %s done" file)))

;;html and javascript editing

(add-to-list 'load-path "~/.emacs.d/elpa/multi-web-mode-0.1/")
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags 
  '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
    (js-mode  "<script[^>]*>" "</script>")
    (css-mode "<style[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)
