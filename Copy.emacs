;; LOAD CHANGES TO .emacs WITHOUT REBOOTING EMACS
;; Start a new emacs session and test whatever changes you made to see if they work correctly. The reason to do it this way is to avoid leaving you in a state where you have an inoperable .emacs file, which fails to load or fails to load cleanly. If you do all of your editing in the original session, and all of your testing in a new session, you'll always have something reliable to comment out offending code. When you are finally happy with your changes, then go ahead and highlight the section you've added/changed and call M-x eval-region. Doing that minimizes the code that's evaluated. 

;;;;;;;;;;;;;;;;;;;;;;;
;; F12 is join lines ;;
;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [f12] 'join-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line-wrap, use F6 to toggle ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-visual-line-mode)
(global-set-key [f6] 'global-visual-line-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Goto-line short-cut key (trumps C-l move to mid-screen) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-l" 'goto-line)

;;;;;;;;;;;;;;;;;;;
;; Linum mode on ;;
;;;;;;;;;;;;;;;;;;;
(require 'linum)
(global-linum-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maximize emacs on startup ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(add-hook 'window-setup-hook 'toggle-frame-maximized t)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delete selection when pressing [delete] key ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(delete-selection-mode t)

;;;;;;;;;;;;;;;;;;
;; Color themes ;;
;;;;;;;;;;;;;;;;;;
(when (< emacs-major-version 27)
  (package-initialize))
(load-theme 'tango-dark t)
;; (custom-theme-set-faces
;;    'tango-dark
;;    '(Man-overstrike ((t (:foreground "red3" :bold t))) t)
;;    '(Man-underline ((t (:foreground "green3" :underline t))))
;;    ;; ... ignored
;;    '(yas-field-highlight-face ((t (:background "#D4DCD8" :foreground "black" :box (:line-width -1 :color "#838383"))))))
(enable-theme 'tango-dark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remember cursor location from last edit ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(save-place-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; will allow you to type just "y" instead of "yes" when you exit ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(fset 'yes-or-no-p 'y-or-n-p) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; will make the last line end in a carriage return ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq require-final-newline t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; will disallow creation of new lines when you press the "arrow-down key" at end of the buffer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq next-line-add-newlines nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; will highlight matching parentheses next to cursor ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'paren) (show-paren-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; will introduce spaces instead of tabs by default ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; will make "Ctrl-k" kill an entire line if the cursor is at the beginning of line -- very useful ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq kill-whole-line t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assigns F5 to (removing the confirmation prompt) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [f5] (lambda () (interactive) (revert-buffer nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package manager for emacs ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "/home/eric/.emacs.d/package")
(require 'package)
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")) # marmalade dead https://emacs.stackexchange.com/questions/10500/do-i-still-need-gnu-elpa-if-i-have-melpa/10501#10501
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Builds list of recently opened files. List automatically saved across sessions on ;;
;; exiting Emacs --- you can then access this list through a command or the menu.    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;;;;;;;;;;;;;;;;;;
;; auto-complete ;;
;;;;;;;;;;;;;;;;;;;
(ac-config-default)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(synosaurus mw-thesaurus exwm ox-gfm auctex auto-complete)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;
;; ess ;;
;;;;;;;;;
(add-to-list 'load-path "/usr/share/emacs/site-lisp/elpa-src/ess-18.10.2/")
(load "ess-autoloads") ; load ess when needed (faster emacs startup)
(setq ess-eval-visibly-p nil) ;otherwise C-c C-r (eval region) takes forever
(setq ess-ask-for-ess-directory nil) ;otherwise you are prompted each time you start an interactive R session
(setq ess-use-auto-complete t) ;initialize auto-complete
;; ess colors
(set-face-attribute 'ac-candidate-face nil :background "#00222c"    :foreground "light gray")
(set-face-attribute 'ac-selection-face nil :background "SteelBlue4" :foreground "white")
(set-face-attribute 'popup-tip-face    nil :background "#003A4E"    :foreground "light gray")
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; smart underscore in R buffers ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'ess-r-mode)
;; (define-key ess-r-mode-map "_" #'ess-insert-assign)
;; (define-key inferior-ess-r-mode-map "_" #'ess-insert-assign)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Call auctex macros ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(setq TeX-auto-save t) 
(setq TeX-parse-self t) 
(setq TeX-save-query nil)
(setq TeX-PDF-mode t)
;; Outline mode (to fold parts of document)
(defun turn-on-outline-minor-mode () (outline-minor-mode 1))
(add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode) 
(add-hook 'latex-mode-hook 'turn-on-outline-minor-mode) 
(setq outline-minor-mode-prefix "\C-c \C-o") ; Or something else

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Avoid auctex TeX-next-error fail ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq LaTeX-command-style '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)")))

;;;;;;;;;;;;
;; RefTex ;;
;;;;;;;;;;;;
;(require 'tex-site) ;; not needed in emacs.25, auctex loaded by default, it seems
(autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase Mode" t)
(add-hook 'latex-mode-hook 'turn-on-reftex) ; with Emacs latex mode
;; (add-hook 'reftex-load-hook 'imenu-add-menubar-index)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Call RefTex to connect to bibtex file ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq reftex-plug-into-AUCTeX t)
;; Make NatBib default C-c [
(setq reftex-cite-format 'natbib)
;;; Add bibtex mode unconditionally; it is already				    
;;; bound to text-mode and we can do better than that.
;; from http://www.sci.utah.edu/~macleod/latex/bibtex-tips.html
(delq (assoc "\\.bib$" auto-mode-alist) auto-mode-alist)			    ;;
(setq auto-mode-alist				 				    ;;
      (cons (cons "\\.bib$" 'bibtex-mode) auto-mode-alist))			    ;;
(autoload 'bibtex-mode  "bibtex"		 				    ;;
  "Enter BibTeX mode for bibliography editing." t nil)				    ;;
						 				    ;;
(setq bibtex-mode-hook '(lambda ()		 				    ;;
                          (setq comment-end "")	 				    ;;
                          (setq comment-start "%% ")				    ;;
                          (setq comment-start-skip "%+ *")			    ;;
                          (setq bibtex-include-OPTcrossref nil)			    ;;
                          (setq bibtex-include-OPTkey nil)			    ;;
                          (setq bibtex-include-OPTannote nil)			    ;;
                          (setq bibtex-include-robnote t)			    ;;
                          (load "/usr/share/emacs/23.3/lisp/bibtex/bibtex-support") ;;
                          (load "/usr/share/emacs/23.3/lisp/bibtex/bibtools")	    ;;
                          ))			 				    ;;

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Synosaurus: parece buena alternativa, pero no consigo que jale... falta wn ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'synosaurus)
;; (setq synosaurus-choose-method "popup")
;; (global-set-key [(control c) (s) (l)] 'synosaurus-lookup)
;; (setq synosaurus-backend 'synosaurus-backend-wordnet)

;;;;;;;;;;;;;;;;;
;; Spell check ;;
;;;;;;;;;;;;;;;;;
;; Use M-x ispell-buffer
;; `DIGIT' Select a near miss
;; `i'     Insert into private dictionary
;; `a'	   Accept for this session
;; `SPC'   Skip this time
;; `r'     Replace with one or more words
;; `l'     Lookup: search the dictionary using a regular expression
;; Use M-x ispell-change-dictionary RET SPC to see list of installed dictionaries
;; Use M-x flyspell to mark typos in whole document (works in LaTex code) see https://www.emacswiki.org/emacs/FlySpell
;; 
;; For more information, see the section on ispell in the emacs manual or use the info system within emacs to look it up (i.e., C-h i and then search for ispell).
;;
;; from http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
;; if (aspell installed) { use aspell}
;; else if (hunspell installed) { use hunspell }
;; whatever spell checker I use, I always use English dictionary
;; I prefer use aspell because:
;; 1. aspell is older
;; 2. looks Kevin Atkinson still get some road map for aspell:
;; @see http://lists.gnu.org/archive/html/aspell-announce/2011-09/msg00000.html
(defun flyspell-detect-ispell-args (&optional run-together)
  "if RUN-TOGETHER is true, spell check the CamelCase words."
  (let (args)
    (cond
     ((string-match  "aspell$" ispell-program-name)
      ;; Force the English dictionary for aspell
      ;; Support Camel Case spelling check (tested with aspell 0.6)
      (setq args (list "--sug-mode=ultra" "--lang=en_US"))
      (if run-together
          (setq args (append args '("--run-together" "--run-together-limit=5" "--run-together-min=2")))))
     ((string-match "hunspell$" ispell-program-name)
      ;; Force the English dictionary for hunspell
      (setq args "-d en_US")))
    args))

(cond
 ((executable-find "aspell")
  ;; you may also need `ispell-extra-args'
  (setq ispell-program-name "aspell"))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")

  ;; Please note that `ispell-local-dictionary` itself will be passed to hunspell cli with "-d"
  ;; it's also used as the key to lookup ispell-local-dictionary-alist
  ;; if we use different dictionary
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))
 (t (setq ispell-program-name nil)))

;; ispell-cmd-args is useless, it's the list of *extra* arguments we will append to the ispell process when "ispell-word" is called.
;; ispell-extra-args is the command arguments which will *always* be used when start ispell process
;; Please note when you use hunspell, ispell-extra-args will NOT be used.
;; Hack ispell-local-dictionary-alist instead.
(setq-default ispell-extra-args (flyspell-detect-ispell-args t))
;; (setq ispell-cmd-args (flyspell-detect-ispell-args))
(defadvice ispell-word (around my-ispell-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))

(defadvice flyspell-auto-correct-word (around my-flyspell-auto-correct-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    ;; use emacs original arguments
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    ;; restore our own ispell arguments
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))

(defun text-mode-hook-setup ()
  ;; Turn off RUN-TOGETHER option when spell check text-mode
  (setq-local ispell-extra-args (flyspell-detect-ispell-args)))
(add-hook 'text-mode-hook 'text-mode-hook-setup)

;; ebib bibtex file manager
(autoload 'ebib "ebib" "Ebib, a BibTeX database manager." t)

;; So that RefTeX finds my bibliography
(setq reftex-default-bibliography '("~/Dropbox/mydocs/magar.bib"))
;; So that RefTeX also recognizes \addbibresource. Note that you
;; can't use $HOME in path for \addbibresource but that "~"
;; works.
(setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))

;; ;; thesaurus
;; (add-to-list 'load-path "/home/eric/.emacs.d/mthesaur")
;; (require 'mthesaur)
;; (global-set-key "\C-ct" 'mthesaur-search)
;; (global-set-key "\C-c\C-t" 'mthesaur-search-append)
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(ebib-preload-bib-files (quote ("~/Dropbox/mydocs/magar.bib")))
;;  '(org-agenda-files (quote ("~/Desktop/mydocs/otherPeoplesWork/referee/chileElRef2015laps.org" "~/Dropbox/mydocs/emagar.org" "~/Desktop/git.org"))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vertical split shows more of each line, horizontal split shows more lines. This code toggles between them. ;;
;; It only works for frames with exactly two windows. Call: F8                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))
(global-set-key [f8] 'my-toggle-window-split)

;; ;; Add google-this (https://github.com/Bruce-Connor/emacs-google-this); now in melpa
;; ;; The main function is `google-this' (bound to C-x g t). It does a
;; ;; google search using the currently selected region, or the
;; ;; expression under point. All functions are bound under "C-x g", to
;; ;; see all keybindings type "C-x g C-h". 
;; ;; Other options described in google-this.el preamble.
;; (require 'google-this)
;; (google-this-mode 1)

;; ;; lisp to count words
;;     (defun count-words (start end)
;;        (interactive "r")
;;        (save-excursion
;;           (let ((n 0))
;;            (goto-char start)
;;            (while (< (point) end)
;;              (if (forward-word 1)
;;                  (setq n (1+ n))))
;;            (message "Region has %d words" n)
;;            n))) 
;; (defalias 'word-count 'count-words)

;; ;; Join lines in region function
;; (defun join-region (beg end)
;;   "Apply join-line over region."
;;   (interactive "r")
;;   (if mark-active
;;           (let ((beg (region-beginning))
;;                         (end (copy-marker (region-end))))
;;                 (goto-char beg)
;;                 (while (< (point) end)
;;                   (join-line 1))))) 

;; org-mode
(require 'org)
;; (define-key global-map "\C-cl" 'org-store-link)
;; (define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; Org-mode settings
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; (global-set-key "\C-cl" 'org-store-link)
;; (global-set-key "\C-ca" 'org-agenda)
;; (global-font-lock-mode 1)

;; ;; Org-mode to LaTex settings (http://orgmode.org/worg/org-tutorials/org-latex-export.html)
;; ;; -> To export, C-c C-e
;; (require 'ox-latex)
;; (unless (boundp 'org-latex-classes)
;;   (setq org-latex-classes nil))
;; (add-to-list 'org-latex-classes
;;              '("article"
;;                "\\documentclass{article}"
;;                ("\\section{%s}" . "\\section*{%s}")))
;; ;;              '("article"
;; ;;                "\\documentclass{article}"
;; ;;                ("\\section{%s}" . "\\section*{%s}")
;; ;;                ("\\subsection{%s}" . "\\subsection*{%s}")
;; ;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;; ;;                ("\\paragraph{%s}" . "\\paragraph*{%s}")
;; ;;                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; ;; enables ffap open file at point https://www.gnu.org/software/emacs/manual/html_node/emacs/FFAP.html#index-ffap-4620
;; (require 'ffap)
;; (ffap-bindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; configures org-publish-project to export multifile pages to html in one step ;;
;; See http://orgmode.org/worg/org-tutorials/org-publish-html-tutorial.html     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ox-publish)
(setq org-publish-project-alist
      '(

  ("org-notes"  ; notes component publishes org files to html
          ;; Path to your org files.
          :base-directory "~/Dropbox/webPage/org/"
          :base-extension "org"
          :publishing-directory "~/Dropbox/webPage/emagar.github.io/"
          :recursive t ; if true exports/creates subdirectories in publishing directory
          :publishing-function org-html-publish-to-html
          :headline-levels 4
          :html-extension "html"
          :body-only t ;; Only export section between <body> </body>
    )

    ("org-static"  ; copies images, tables etc to publishing-directory; creates non-existant dirs to match base-directory's structure 
          :base-directory "~/Dropbox/webPage/org/"
          :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php\\|xls\\|xlsx"
          :publishing-directory "~/Dropbox/webPage/emagar.github.io/"
          :recursive t
          :publishing-function org-publish-attachment)

    ("org" :components ("org-notes" "org-static")) ; publish all with single command: M-x org-publish-project RET org RET
                                                    ; or C-c e X org 
    ))
; tells babel how to export yaml front matter for jekyll
(defun org-babel-execute:yaml (body params) body)

;; Properly follow org-mode links when exporting to Jekyll
;; inspired from http://cute-jumper.github.io/emacs/2013/10/06/orgmode-to-github-pages-with-jekyll
;; Usage: When we want to link to a post, we now use the syntax: [[jekyll-post:filename][description]] instead.
(defun org-jekyll-post-link-follow (path)
  (org-open-file-with-emacs path))

(defun org-jekyll-post-link-export (path desc format)
  (cond
   ((eq format 'html)
    (setq path (replace-regexp-in-string ".org" "" path))
    (format "<a href=\"{{ site.baseurl }}{%% post_url %s %%}\">%s</a>" path desc))))

(org-add-link-type "jekyll-post" 'org-jekyll-post-link-follow 'org-jekyll-post-link-export)


;; ;; ;; Version used before migrating to Jekyll
;; ;; (require 'ox-publish)
;; ;; (setq org-publish-project-alist
;; ;;       '(

;; ;;        ;; ... add all the components here (see below)...
;; ;; ("org-notes"
;; ;;  :base-directory "~/Dropbox/data/elecs/MXelsCalendGovt/reelec/webPage/"
;; ;;  :base-extension "org"
;; ;;  :publishing-directory "~/Dropbox/data/elecs/MXelsCalendGovt/reelec/webPage/publishThis/"
;; ;;  :recursive t  ; if true exports/creates subdirectories in publishing directory
;; ;;  :publishing-function org-html-publish-to-html
;; ;;  :headline-levels 3             ; Just the default for this project.
;; ;;  :auto-preamble t
;; ;;  :auto-sitemap t                  ; Generate sitemap.org automagically...
;; ;;  :sitemap-filename "sitemap.org"  ; ... call it sitemap.org (it's the default)...
;; ;;  :sitemap-title "Mapa de sitio"   ; ... with title...
;; ;;  )

;; ;; ("org-static" ; copies images, tables etc to publishing directory towards publication
;; ;;  :base-directory "~/Dropbox/data/elecs/MXelsCalendGovt/reelec/webPage/"
;; ;;  :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
;; ;;  :publishing-directory "~/Dropbox/data/elecs/MXelsCalendGovt/reelec/webPage/publishThis/"
;; ;;  :recursive t
;; ;;  :publishing-function org-publish-attachment
;; ;;  )

;; ;; ("org" :components ("org-notes" "org-static")) ; publish all with single command: M-x org-publish-project RET org RET

;; ;; ; directory structure
;; ;; ~/Dropbox/data/elecs/MXelsCalendGovt/reelec/webPage/
;; ;;   |- css/
;; ;;   |  `- stylesheet.css
;; ;;   |- Emacs
;; ;;   |  |- index.org
;; ;;   |  |- gnus.org
;; ;;   |  |- org.org
;; ;;   |  `- snippets.org
;; ;;   |- img/
;; ;;   |- index.org
;; ;;   `- remember.org

;; ;;       ))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; END configure org-publish-project to export multifile pages to html in one step ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ; org export to beamer
;; (require 'ox-latex)
;; (add-to-list 'org-latex-classes
;;              '("beamer"
;;                "\\documentclass\[presentation\]\{beamer\}"
;;                ("\\section\{%s\}" . "\\section*\{%s\}")
;;                ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
;;                ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))


;; ;; ; org format timestamps http://endlessparentheses.com/better-time-stamps-in-org-export.html
;; ;; (add-to-list 'org-export-filter-timestamp-functions
;; ;;              #'endless/filter-timestamp)
;; ;; (defun endless/filter-timestamp (trans back _comm)
;; ;;   "Remove <> around time-stamps."
;; ;;   (pcase back
;; ;;     ((or `jekyll `html)
;; ;;      (replace-regexp-in-string "&[lg]t;" "" trans))
;; ;;     (`latex
;; ;;      (replace-regexp-in-string "[<>]" "" trans))))
;; ;; ; still need way to bypass locale date language...
;; ;; (setq-default org-display-custom-times t)
;; ;; (setq org-time-stamp-custom-formats
;; ;;       '("<%d %b %Y>" . "<%d/%m/%y %a %H:%M>"))



;; By an unknown contributor: jumps to matching parenthesis/bracket
(global-set-key "%" 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))


;; Github Flavored Markdown exporter for Org Mode
;; https://github.com/larstvei/ox-gfm
(eval-after-load "org"
  '(require 'ox-gfm nil t))

;; ;; Markdown exporter for Org Mode
;; (eval-after-load "org"
;;   '(require 'ox-md nil t))


;; ;; exwm emacs window manager 
;; ;; see https://github.com/ch11ng/exwm/wiki#bootstrap
;; (exwm-enable)

;; ;; exwm global key bindings
;; (setq exwm-input-prefix-keys
;;       `(([?\s-r] . exwm-reset)            ; switch to line mode
;;         ([?\s-w] . exwm-workspace-switch) ; interactively switch to next workspace
;;         ,@(mapcar (lambda (i)             ; switch to workspace 1-9
;;                     `(,(kbd (format "s-%d" i)) .
;;                       (lambda ()
;;                         (interactive)
;;                         (exwm-workspace-switch-create ,i))))
;;                   (number-sequence 0 9))))

