;; set up the appearance of emacs at startup
;;
;; Drop menus and toolbars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-window-fringes nil 0 0)
;;(toggle-frame-fullscreen)
;;
;; Add the package archives
;; (package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
;;
;; Setup the startup windows
;;
;;
(global-set-key (kbd "C-c a") 'org-agenda)
(split-window-right)
(dired "~/projects/")
(dired-hide-details-mode)
(text-scale-set -1)
(other-window 1)
;;(find-file "~/projects/schedule.org")
(load-theme 'misterioso)
;;
;; Get rid of splash screen
;;
(setq inhibit-splash-screen t)
;;
;; Don't use messages that you don't read
;;
(setq initial-scratch-message "")
;;
;; Add column  numbers
(setq column-number-mode t)
;; Section 1: Important stuff
;;
;; Backup file location
;;
(if (file-exists-p "~/.saves")
    nil
  (make-directory "~/.saves"))
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
;;
;; Add a hook to delete trailing white space on save
;;
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Set the default Dictionary
(setq ispell-dictionary "british")
(setq ispell-personal-dictionary "~/.personal-dictionary")
;;
;; encoding of files UTF-8
(prefer-coding-system 'utf-8)
(setq coding-system-for-write 'utf-8)
;;
;; Delete files by moving them to trash
;;
(setq delete-by-moving-to-trash t)
;;
;; Fixed the dead tilda in ubuntu
(require 'iso-transl)
;;
;; cycle forward in the kill ring
(defun yank-pop-forwards (arg)
      (interactive "p")
      (yank-pop (- arg)))
(global-set-key "\M-Y" 'yank-pop-forwards) ; M-Y (Meta-Shift-Y)
;;
;; Section 2: Other stuff, packages loaded
;;
;; R startup options
(setq inferior-R-args "--no-restore-history --no-save")
;;
;; shortcuts
(global-set-key (kbd "C-c r") 'R)
(global-set-key (kbd "C-c s") 'shell)
(global-set-key (kbd "<VoidSymbol>") 'compile)
(global-set-key (kbd "<f5>") 'magit-status)
(global-set-key (kbd "<Scroll_Lock>") 'make-frame-command)
(global-set-key (kbd "C-x C-b") 'ibuffer)
;;
;; Autocomplete
;;
;(require 'auto-complete-config)
(require 'auto-complete)
(ac-config-default)
(setq ess-tab-complete-in-script t)
;;
;; Multiple cursors
;;
(require 'multiple-cursors)
;;
;; Latex compilation and preview
;;
(add-to-list 'load-path "~/.emacs.d/elpa/auctex-12.1.1/")
(load "auctex.el" nil t t)
(require 'tex-site)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-buffer)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(setq LaTeX-item-indent 0)
(setq LaTeX-indent-level 2)
(setq TeX-brace-indent-level 2)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq TeX-parse-self t) ; Enable parse on load.
(setq TeX-auto-save t) ; Enable parse on save.
;;
;; Markdown mode
(require 'markdown-mode)
;;
;; Add yasnippet
;;
(add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-20180621.50/")
(add-to-list 'load-path "~/.emacs.d/elpa/auto-complete-auctex-20140223.1758/")
(load "yasnippet.el" nil t t)
(load "auto-complete-auctex.el" nil t t)
(require 'auto-complete-auctex)
;;
;; Add flyspell to markdown mode
;;
(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'flyspell-buffer)
(add-hook 'markdown-mode-hook 'auto-fill-mode)
;;
;; Allow a to be used in dired mode
;;
(put 'dired-find-alternate-file 'disabled nil)
;;
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(nil nil t)
;;  '(org-agenda-files
;;    (quote
;;     ("~/projects/schedule.org")))
;;  '(package-selected-packages
;;    (quote
;;     (indium polymode org multiple-cursors multi-web-mode markdown-mode magit ess-R-object-popup dired+ auto-complete-auctex auctex)))
;;  '(send-mail-function (quote smtpmail-send-it))
;;  '(smtpmail-smtp-server "localhost")
;;  '(smtpmail-smtp-service 1025))
;;
;; Email address
;;
(setq user-mail-address "thomas.rosendal@sva.se")
(setq user-full-name "Thomas Rosendal")
;;
;; gnus mail setup
;; (require 'gnus)
;; (setq nnml-directory "~/mail")
;; (setq message-directory "~/mail")
;; (setq gnus-select-method
;;       '(nnimap "davmail"
;;                (nnimap-address "localhost")
;;                (nnimap-server-port 1143)
;;                (nnimap-stream ssl)))
;; (setq gnus-thread-sort-functions
;;       '(gnus-thread-sort-by-number
;; 	(not gnus-thread-sort-by-date)))
;;
;; resize windows quickly
;;
(global-set-key (kbd "C-M-6") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-7") 'shrink-window-horizontally)
;;
;; Set line spacing quickly
;;
(defun add-line-space ()
  "Add a half to line space."
  (interactive)
  (if (eq line-spacing nil)
      (progn
	(setq line-spacing 0.5)
	(message (number-to-string line-spacing)))
    (progn
      (setq line-spacing (+ line-spacing 0.5))
      (message (number-to-string line-spacing)))))
(defun subtract-line-space ()
  "subtract a half from line space."
  (interactive)
  (if (< line-spacing 0.1)
      (message (number-to-string line-spacing))
    (progn
      (setq line-spacing (- line-spacing 0.5))
      (message (number-to-string line-spacing)))))
(global-set-key (kbd "C-M-8") 'add-line-space)
(global-set-key (kbd "C-M-9") 'subtract-line-space)
;;
;; Focus on a buffer
;;
(defun focus-on-emacs ()
  "drop everything and make full screen"
  (interactive)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (set-window-fringes nil 0 0))
(global-set-key (kbd "C-<f11>") 'focus-on-emacs)
;; UnFocus on a buffer
(defun unfocus-on-emacs ()
  "drop everything and make full screen"
  (interactive)
  (menu-bar-mode 1)
  (tool-bar-mode 1)
  (scroll-bar-mode 1)
  (set-window-fringes nil 10 0))
(global-set-key (kbd "C-x <C-f11>") 'unfocus-on-emacs)
;;
;; Polymode for rmd files
(require 'poly-R)
(require 'poly-markdown)
(require 'ess-site)
(autoload 'r-mode "ess-site" "(Autoload)" t)
;; MARKDOWN
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
;; R modes
(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
;;
;; Function to custom open pdfs with evince or other files like gnome
;; does when you double click on them. Requires the software 'gnome-open'
;;
(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "gnome-open" nil 0 nil file)
    (message "Opening %s done" file)))
(global-set-key (kbd "<f4>") 'dired-open-file)
;;
;; Navigate up to parent directory in dired with C-.  The default
;; keyboard shortcut is annoying on the Swedish keyboard because the ^
;; key requires you to press S-^ <space>.
;;
;; Note the definition to the dired mode only *after* loading dired
;;
(eval-after-load 'dired
                    '(define-key dired-mode-map (kbd "C-.") 'dired-up-directory))
;;
;; html and javascript editing
;;
(add-to-list 'load-path "~/.emacs.d/multi-web-mode/")
(load "multi-web-mode.el" nil t t)
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags
  '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
    (js-mode  "<script[^>]*>" "</script>")
    (css-mode "<style[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)
;;
;;
;; Get the svar.el package from somewhere and load it
;; (load-file "~/.emacs.d/lisp/svar/svar.el")
;;
;;
;; SECTION 3: Other junk that I don't use but put here because
;;
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word)
  )
(global-set-key (kbd "<f8>") 'flyspell-check-next-highlighted-word)
;;Org-mode
;;(add-to-list 'load-path "/usr/share/emacs/site-lisp/org-mode/")
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
        )
      )
;;Add a feature to copy a line from anywhere in the line
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
(defun tr/cleanup ()
  "find plus not preceeded by space and fixes it "
  (interactive)
  (save-excursion
    ;; Replace +
    (goto-char (point-min))
    (while (re-search-forward "[[:space:]]*\\([+]\\)[[:space:]]*" nil t)
      (replace-match " \\1 ")
      )
    ;; Replace *
    (goto-char (point-min))
    (while (re-search-forward "[[:space:]]*\\([*]\\)[[:space:]]*" nil t)
      (replace-match " \\1 ")
      )
    ;; Replace -
    (goto-char (point-min))
    (while (re-search-forward "[[:space:]]*\\([-]\\)[[:space:]]*" nil t)
      (replace-match " \\1 ")
      )
    ;;
    )
  )
;; ;; Check for number gaps
;; ;; query-replace-regexp
;; ;; \([0-9]\)\s-\([0-9]\)
;; ;; \1\2

;; ;; Check for numbers with commas
;; ;; query-replace-regexp
;; ;; \([0-9]\),\([0-9]\)
;; ;; \1\2

;; ;; Check for numbers with tildas
;; ;; query-replace-regexp
;; ;; \([0-9]\)~\([0-9]\)
;; ;; \1\2

;; ;; Replace ä
;; ;; query-replace-regexp
;; ;; ä
;; ;; {\"a}

;; ;; Replace å
;; ;; query-replace-regexp
;; ;; å
;; ;; {\aa}

;; ;; Replace ö
;; ;; query-replace-regexp
;; ;; ö
;; ;; {\"o}

;; ;; ;http://stackoverflow.com/questions/8334263/in-emacs-how-to-replace-only-on-matching-lines
;; ;; (defun my-replace-on-matching-lines (&optional arg)
;; ;;   "Replace text on lines that match a regexp.
;; ;; With prefix arg, replace on non-matching lines."
;; ;;   (interactive "P")
;; ;;   (let* ((regexp (concat ".*"
;; ;;                          (read-from-minibuffer
;; ;;                           (concat "Replace on lines "
;; ;;                                   (if arg "not " "")
;; ;;                                   "matching regexp: "))))
;; ;;          (replace (read-from-minibuffer "Replace: "))
;; ;;          (with (read-from-minibuffer (concat "Replace " replace " with: ")))
;; ;;          match)
;; ;;     (save-excursion
;; ;;       (goto-char (point-min))
;; ;;       (while (not (eobp))
;; ;;         (setq match (looking-at regexp))
;; ;;         (when (if arg (not match) match)
;; ;;           (while (search-forward replace (point-at-eol) t)
;; ;;             (replace-match with nil t)))
;; ;; 	(add-text-properties (match-beginning 1)
;; ;; 			     (match-end 1)
;; ;; 			     (list 'face 'bold))
;; ;;         (forward-line)))))
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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;
;; C code style
;;
(setq c-default-style "k&r"
      c-basic-offset 4)
(setq-default indent-tabs-mode nil)
