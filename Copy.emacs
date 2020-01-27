;; LOAD CHANGES TO .emacs WITHOUT REBOOTING EMACS
;; Start a new emacs session and test whatever changes you made to see if they work correctly. The reason to do it this way is to avoid leaving you in a state where you have an inoperable .emacs file, which fails to load or fails to load cleanly. If you do all of your editing in the original session, and all of your testing in a new session, you'll always have something reliable to comment out offending code. When you are finally happy with your changes, then go ahead and highlight the section you've added/changed and call M-x eval-region. Doing that minimizes the code that's evaluated. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dead-acute became undefined when updating to ubuntu 14.04 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'iso-transl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; where installed packages reside (EM: no need to include subirectories below, it seems) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "/home/eric/.emacs.d/elpa/") 

;; package manager for emacs
;; To run: call list-packages, see http://ergoemacs.org/emacs/emacs_package_system.html
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

;; Recent files in directory
(require 'recentf)
(recentf-mode 1)
(put 'narrow-to-region 'disabled nil)
(setq tex-dvi-view-command "xdvi")

;;;;;;;;;;;;;;;;;;;;;;;
;; F12 is join lines ;;
;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [f12] 'join-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Goto-line short-cut key (trumps C-l move to mid-screen) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-l" 'goto-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line-wrap, use F8 to toggle ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-visual-line-mode)
(global-set-key [f8] 'global-visual-line-mode)

;;;;;;;;;;;;;;;;;;;
;; Linum mode on ;;
;;;;;;;;;;;;;;;;;;;
(require 'linum)
(global-linum-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maximize emacs on startup ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; below 2 lines are older way, which does not travel across screens... drop if new version (added 13feb16) works ok
;; (set-frame-position (selected-frame) 0 0)
;; (set-frame-size (selected-frame) 237 65)

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
(setq outline-minor-mode-prefix "\C-c\C-o") ; Or something else

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Avoid auctex TeX-next-error fail ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq LaTeX-command-style '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)")))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Synosaurus: parece buena alternativa, pero no consigo que jale... falta wn ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'synosaurus)
(setq synosaurus-choose-method "popup")
(global-set-key [(control c) (s) (l)] 'synosaurus-lookup)
(setq synosaurus-backend 'synosaurus-backend-wordnet)

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

;;  ;; predictive install location <-- COMMENTED IT, WASNT LETTING AUTO-COMPLETE WORK 
;; (add-to-list 'load-path "/home/eric/.emacs.d/gitPackages/predictive")
;; ;; dictionnary locations
;; (add-to-list 'load-path "/home/eric/.emacs.d/gitPackages/predictive/html")
;; (add-to-list 'load-path "/home/eric/.emacs.d/gitPackages/predictive/latex")
;; (add-to-list 'load-path "/home/eric/.emacs.d/gitPackages/predictive/texinfo")
;; ;; load predictive package
;; (require 'predictive)
;; ;;(autoload 'predictive-mode "~/.emacs.d/gitPackages/predictive/predictive" ;; WOULD SAVE MEMORY
;; ;;               "Turn on Predictive Completion Mode." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONFIGURE ELPA'S AUTO-COMPLETE ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TUVE QUE MODIFICAR auto-complete.el PARA QUE NO TARDARA 10 SEGS EN CERRAR
;;    ver http://superuser.com/questions/795654/quitting-emacs-takes-10-seconds/796506#796506
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(require 'ac-math) 
(add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of `latex-mode`
 (defun ac-LaTeX-mode-setup () ; add ac-sources to default ac-sources
   (setq ac-sources
         (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
                 ac-sources))
   )
(add-hook 'LaTeX-mode-hook 'ac-LaTeX-mode-setup)
(global-auto-complete-mode t)
(setq ac-math-unicode-in-math-p t)

;; obsoleto con 24.5; tbién parece serlo con 24.3
;; ;; ESTE BLOQUE ERA RESPONSABLE DE QUE QUIT TARDARA 15 SEGUNDOS... TUVE QUE MODIFICAR auto-complete.el
;; ;;auto-complete
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "/home/eric/.emacs.d/ac-dict")
;; (ac-config-default)
;; (define-key ac-completing-map "\t" 'ac-complete) ;; change return key with tab to ac
;; (define-key ac-completing-map [tab] 'ac-complete) ;; With recent versions of auto-complete you may need to set
;; (define-key ac-completing-map [return] nil)       ;; With recent versions of auto-complete you may need to set
;;
;; (require 'auto-complete-latex)
;; ;(setq ac-l-dict-directory "/home/eric/.emacs.d/ac-l-dict/")
;; (setq ac-l-dict-directory "/home/eric/.emacs.d/elpa/auto-complete-20160107.8/dicta/c-l-dict/")
;; ;;(add-to-list 'ac-modes 'foo-mode)
;; ;;(add-hook 'foo-mode-hook 'ac-l-setup)
;; (require 'ac-math)
;; (add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of {{{latex-mode}}}
;; (defun ac-latex-mode-setup ()         ; add ac-sources to default ac-sources
;;   (setq ac-sources
;;      (append '(ac-source-math-latex ac-source-latex-commands  ac-source-math-unicode)
;;                ac-sources))
;; )
;; ;;(require 'auto-complete)
;; (add-hook 'LaTeX-mode-hook 'ac-latex-mode-setup)
;; ;;(ac-flyspell-workaround)        ; workaround if using flyspell

;; obsoleto desde antes de 24.5
;; from http://code.google.com/p/ac-math/
;; This is an add-on which defines three ac-sources for the auto-complete package:
;; 1 ac-source-latex-commands - input latex commands
;; 2 ac-source-math-latex - input math latex tags (active only in math environments) 
;; 3 ac-source-math-unicode - input of unicode symbols (active everywhere except math environments) 
;(require 'ac-math)
;(add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of {{{latex-mode}}}
;(defun ac-latex-mode-setup ()         ; add ac-sources to default ac-sources
;  (setq ac-sources
;     (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
;               ac-sources))
;)
;(add-hook 'LaTeX-mode-hook 'ac-latex-mode-setup)
;(ac-flyspell-workaround)

;;;;;;;;;
;; ess ;;
;;;;;;;;;
(setq ess-eval-visibly-p nil) ;otherwise C-c C-r (eval region) takes forever
(setq ess-ask-for-ess-directory nil) ;otherwise you are prompted each time you start an interactive R session
(setq ess-use-auto-complete t) ;initialize auto-complete
;;;;;;;;;;;;;;;;
;; ess colors ;;
;;;;;;;;;;;;;;;;
(set-face-attribute 'ac-candidate-face nil   :background "#00222c" :foreground "light gray")
(set-face-attribute 'ac-selection-face nil   :background "SteelBlue4" :foreground "white")
(set-face-attribute 'popup-tip-face    nil   :background "#003A4E" :foreground "light gray")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smart underscore in R buffers ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ess-r-mode)
(define-key ess-r-mode-map "_" #'ess-insert-assign)
(define-key inferior-ess-r-mode-map "_" #'ess-insert-assign)

;; Delete selection when pressing [delete] key
(delete-selection-mode t)

;; ;; Seems redundant in emacs 24 given ELPA...
;; ;; Auto-install lisp files
;; ;; http://www.emacswiki.org/emacs/AutoInstall#toc1
;; (add-to-list 'load-path (expand-file-name "/home/eric/.emacs.d/auto-install/"))
;; (setq auto-install-directory "/home/eric/.emacs.d/auto-install/")
;; (require 'auto-install)

;; ;; Color themes
;(require 'color-theme-modern)
(require 'zenburn-theme)
;; ;; Usage M-x color-theme-select
;; ;; http://lifegoo.pluskid.org/wiki/lisp/color-theme.el
;; (require 'color-theme) ; tells Emacs to always enable color-theme mode
;; (color-theme-initialize) ; tells Emacs to load all the color-themes at startup so that you can immediately switch between them if you wish
;; ; este color no jala más... y está bonito
;; (add-to-list 'load-path "/home/eric/.emacs.d/gitPackages/color-theme-calm-forest/")
;; (require 'color-theme-calm-forest) 
;; ;(color-theme-calm-forest) ; tells Emacs to enable Calm Forest color theme
;; (require 'color-theme-cobalt)
;; (color-theme-cobalt) ; tells Emacs to enable Cobalt color theme

;; ;; Remember cursor location from last edit (24.5 and older versions)
;; (require 'saveplace)                                   ;; get the package
;; (setq-default save-place t)                            ;; activate it for all buffers
;; (setq save-place-file "/home/eric/.emacs.d/saveplace") ;; keep my ~/ clean

;; ;; Remember cursor location from last edit (25.1 and newer versions)
(save-place-mode 1)

;; will make the last line end in a carriage return
(setq require-final-newline t)

;; will disallow creation of new lines when you press the "arrow-down key" at end of the buffer
(setq next-line-add-newlines nil)

;; will highlight matching parentheses next to cursor
(require 'paren) (show-paren-mode t)

;; will introduce spaces instead of tabs by default
(setq-default indent-tabs-mode nil)

;; will make "Ctrl-k" kill an entire line if the cursor is at the beginning of line -- very useful
(setq kill-whole-line t)

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


;; Vertical split shows more of each line, horizontal split shows more lines. This code toggles between them. It only works for frames with exactly two windows. Call: C-|
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
(global-set-key [(control c) (|)] 'my-toggle-window-split)

;; Add google-this (https://github.com/Bruce-Connor/emacs-google-this); now in melpa
;; The main function is `google-this' (bound to C-x g t). It does a
;; google search using the currently selected region, or the
;; expression under point. All functions are bound under "C-x g", to
;; see all keybindings type "C-x g C-h". 
;; Other options described in google-this.el preamble.
(require 'google-this)
(google-this-mode 1)

;; lisp to count words
    (defun count-words (start end)
       (interactive "r")
       (save-excursion
          (let ((n 0))
           (goto-char start)
           (while (< (point) end)
             (if (forward-word 1)
                 (setq n (1+ n))))
           (message "Region has %d words" n)
           n))) 
(defalias 'word-count 'count-words)

;; Join lines in region function
(defun join-region (beg end)
  "Apply join-line over region."
  (interactive "r")
  (if mark-active
          (let ((beg (region-beginning))
                        (end (copy-marker (region-end))))
                (goto-char beg)
                (while (< (point) end)
                  (join-line 1))))) 

;; org-mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; ;; bibtex in org files <- NOT WORKING
;; ;; https://emacs.stackexchange.com/questions/3375/loading-bibtex-file-in-org-mode-file
;; (require 'ox-bibtex)
;; (setq org-latex-pdf-process '("texi2dvi -p -b -V %f"))


;; org2blog lets you publish directly to wordpress
;; https://github.com/punchagan/org2blog/blob/master/README.org
(require 'xml-rpc)
(require 'metaweblog)
(require 'org2blog-autoloads)
(require 'netrc) ;; to set login and password in .netrc file
(setq blog (netrc-machine (netrc-parse "~/.netrc") "blog-cipol" t))
(setq org2blog/wp-blog-alist
           ;; '(("wordpress"
           ;;    :url "http://ericmagar.com/blog/xmlrpc.php"
           ;;    :username "emagar@gmail.com"
           ;;    :default-title "Hola mundo"
           ;;    :default-categories ("Congreso" "elecciones")
           ;;    :tags-as-categories nil)))
           '(("el-blog-de-cipol"
              :url "https://ericmagar.wordpress.com/xmlrpc.php"
              :username (netrc-get blog "login")
              :password (netrc-get blog "password")
              :default-title "Titúlelo plis"
              :default-categories ("Congreso" "elecciones")
              :wp-latex t
              :tags-as-categories nil)))
;(setq org2blog/wp-use-sourcecode-shortcode t)
;(setq org2blog/wp-use-wp-latex) ; should allow to use wp's engine to render latex equations

;; Org-mode settings
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-font-lock-mode 1)

;; Org-mode to LaTex settings (http://orgmode.org/worg/org-tutorials/org-latex-export.html)
;; -> To export, C-c C-e
(require 'ox-latex)
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))
(add-to-list 'org-latex-classes
             '("article"
               "\\documentclass{article}"
               ("\\section{%s}" . "\\section*{%s}")))
;;              '("article"
;;                "\\documentclass{article}"
;;                ("\\section{%s}" . "\\section*{%s}")
;;                ("\\subsection{%s}" . "\\subsection*{%s}")
;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;                ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


;; ;; EM 7abr2016: no estoy seguro de seguir queriendo esto... ahora está instalado desde melpa
;; ;; el-get installs el files from sources such as github 
;; ;; See http://wikemacs.org/wiki/El-get
;; ;;     http://bytes.inso.cc/2011/08/13/auto-installing-packages-in-emacs-with-elpa-and-el-get/
;; ;(add-to-list 'load-path "~/.emacs.d/elpa/el-get-20150409.1626/el-get") ; DROP
;; (unless (require 'el-get nil t)
;;   (url-retrieve
;;    "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
;;    (lambda (s)
;;      (end-of-buffer)
;;      (eval-print-last-sexp))))
;; ;; set local recipes, el-get-sources should only accept PLIST element
;; (setq
;;  el-get-sources                          ; list el code to install and load
;;  '((:name ox-s5                          ; export org to s5 slides
;;           :description "Git to s5 slides code"
;;           :type git
;;           :url "git://github.com/cybercode/org-slides.git"
;;           :load "ox-s5.el"
;;           :compile ("ox-s5.el")
;;           :features ox-s5)
;;    ;; (:name buffer-move			; have to add your own keys
;;    ;;        :after (progn
;;    ;;      	   (global-set-key (kbd "<C-S-up>")     'buf-move-up)
;;    ;;      	   (global-set-key (kbd "<C-S-down>")   'buf-move-down)
;;    ;;      	   (global-set-key (kbd "<C-S-left>")   'buf-move-left)
;;    ;;      	   (global-set-key (kbd "<C-S-right>")  'buf-move-right)))
;;    ;; (:name smex				; a better (ido like) M-x
;;    ;;        :after (progn
;;    ;;      	   (setq smex-save-file "~/.emacs.d/.smex-items")
;;    ;;      	   (global-set-key (kbd "M-x") 'smex)
;;    ;;      	   (global-set-key (kbd "M-X") 'smex-major-mode-commands)))
;;    ;; (:name magit				; git meet emacs, and a binding
;;    ;;        :after (progn
;;    ;;      	   (global-set-key (kbd "C-x C-z") 'magit-status)))
;;    ;; (:name goto-last-change		; move pointer back to last change
;;    ;;        :after (progn
;;    ;;      	   ;; when using AZERTY keyboard, consider C-x C-_
;;    ;;      	   (global-set-key (kbd "C-x C-/") 'goto-last-change)))
;;    ))
;; ; list all packages you want installed  
;; (setq my-el-get-packages  
;;       (append  
;;        '(ox-s5)  
;;        (mapcar 'el-get-source-name el-get-sources)))  
;; (el-get 'sync my-el-get-packages)  

;; ;;; Easy bookmarks
;; ;;; Locally defined recipe to el-get breadcrumb from github
;; ;; (el-get-bundle breadcrumb
;; ;;   :url "https://github.com/pheaver/breadcrumb.git"
;; ;;   :features breadcrumb)
;; (add-to-list 'load-path "/home/eric/.emacs.d/el-get/breadcrumb")
;; (require 'breadcrumb)
;; ;; configure
;; (global-set-key [(f2)]                  'bc-set)            ;; Shift-SPACE for set bookmark
;; (global-set-key [(meta j)]              'bc-previous)       ;; M-j for jump to previous
;; (global-set-key [(shift meta j)]        'bc-next)           ;; Shift-M-j for jump to next
;; (global-set-key [(meta up)]             'bc-local-previous) ;; M-up-arrow for local previous
;; (global-set-key [(meta down)]           'bc-local-next)     ;; M-down-arrow for local next
;; (global-set-key [(control c)(j)]        'bc-goto-current)   ;; C-c j for jump to current bookmark
;; (global-set-key [(control x)(meta j)]   'bc-list)           ;; C-x M-j for the bookmark menu list
;; ;
;; ;  Another set of bindings similar to MS Visual Studio bookmark setting.
;; ;  (global-set-key [(control f2)]          'bc-set)
;; ;  (global-set-key [(f2)]                  'bc-previous)
;; ;  (global-set-key [(shift f2)]            'bc-next)
;; ;  (global-set-key [(meta f2)]             'bc-list)


;; reveal for html presentations. See https://github.com/yjwen/org-reveal/blob/master/Readme.org
(require 'ox-reveal)
(setq org-reveal-root "file:///home/eric/.emacs.d/reveal/reveal.js")

;; Assigns F5 to (removing the confirmation prompt)
(global-set-key [f5] (lambda () (interactive) (revert-buffer nil t)))


;; add youtube iframe in org mode easily (usage example: [[yt:A3JAlWM8qRM]])
;; from http://endlessparentheses.com/embedding-youtube-videos-with-org-mode-links.html
(defvar yt-iframe-format
  ;; You may want to change your width and height.
  (concat "<iframe width=\"440\""
          " height=\"335\""
          " src=\"https://www.youtube.com/embed/%s\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe>"))

(org-add-link-type
 "yt"
 (lambda (handle)
   (browse-url
    (concat "https://www.youtube.com/embed/"
            handle)))
 (lambda (path desc backend)
   (cl-case backend
     (html (format yt-iframe-format
                   path (or desc "")))
     (latex (format "\href{%s}{%s}"
                    path (or desc "video"))))))

;; interface for w3m web browser
(add-to-list 'load-path "/usr/share/emacs/site-lisp/w3m") 
(require 'w3m-load)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bbdb-file "~/.emacs.d/.bbdb")
 '(org-agenda-files
   (quote
    ("~/Desktop/mydocs/otherPeoplesWork/referee/review-APSR-D-18-00877-2018.org" "~/Dropbox/mydocs/emagar.org" "~/Dropbox/mydocs/emm/bienesRaices/viento/opciones.org")))
 '(org-export-backends (quote (ascii beamer html icalendar latex md)))
 '(org-support-shift-select t)
 '(package-selected-packages
   (quote
    (zenburn-theme color-theme-modern weather yaml-mode w3m w3 synosaurus rainbow-mode ox-reveal ox-gfm org2blog markdown-mode+ langtool google-this gnu-elpa-keyring-update ess color-theme-complexity color-theme-cobalt bbdb ac-math ac-ispell)))
 '(safe-local-variable-values (quote ((TeX-master . "master"))))
 '(send-mail-function (quote mailclient-send-it))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:background "#111111" :foreground "#888888" :underline nil :height 0.9)))))

;; https://www.gnu.org/software/emacs/manual/html_node/gnus/FAQ-5_002d7.html
;; use bbdb to store/retrieve mail addresses
;;If you don't live in Northern America, you should disable the
;;syntax check for telephone numbers by saying
(setq bbdb-north-american-phone-numbers-p nil)
;;Tell bbdb about your email address:
(setq bbdb-user-mail-names
      (regexp-opt '("emagar@gmail.com"
                    "emagar@itam.mx")))
;;cycling while completing email addresses
(setq bbdb-complete-name-allow-cycling t)
;;No popup-buffers
(setq bbdb-use-pop-up nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bbdb-handy no longer available in melpa?? 31oct19 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; when in mail mode, opens BBDB window when TAB pressed in TO: or CC: fields
;; (require 'bbdb-handy)
;; ;; bbdb-mode
;; (define-key bbdb-mode-map "g" 'bbdb-handy-display-all-records)
;; (define-key bbdb-mode-map "q" 'bbdb-handy-quit-window)
;; (define-key bbdb-mode-map "p" 'bbdb-handy-bbdb-push-mail)
;; (define-key bbdb-mode-map "\C-s" 'bbdb-handy-search-records)
;; (define-key bbdb-mode-map "b" 'bbdb-handy-search-records)
;; (define-key bbdb-mode-map "\C-c\C-c" 'bbdb-handy-push-mail)
;; (define-key bbdb-mode-map (kbd "RET") 'bbdb-handy-push-mail-and-quit-window)
;; ;; Message-mode
;; (define-key message-mode-map "\t" 'bbdb-handy-message-tab)


;; imports gmail vCard as addresses
;; Run command “M-x gmail2bbdb-import-file” and select contacts.vcf. “$HOME/.bbdb” will be created.
;; https://github.com/redguardtoo/gmail2bbdb
(add-to-list 'load-path "~/.emacs.d/lisp/")
(autoload 'gmail2bbdb-import-file "gmail2bbdb" nil t nil)
(setq gmail2bbdb-bbdb-file "~/.emacs.d/.bbdb")                   ; path of output file
;(add-to-list 'gmail2bbdb-excluded-email-regex-list "noreply@.*") ; excluding addresses matching one of the patterns

;; org-mode calendar integration with Edward m. Reingold's diary/caledar
;(setq org-agenda-include-diary t)


;; files that org agenda will search to extract dates etc


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


;; ;; Version used before migrating to Jekyll
;; (require 'ox-publish)
;; (setq org-publish-project-alist
;;       '(

;;        ;; ... add all the components here (see below)...
;; ("org-notes"
;;  :base-directory "~/Dropbox/data/elecs/MXelsCalendGovt/reelec/webPage/"
;;  :base-extension "org"
;;  :publishing-directory "~/Dropbox/data/elecs/MXelsCalendGovt/reelec/webPage/publishThis/"
;;  :recursive t  ; if true exports/creates subdirectories in publishing directory
;;  :publishing-function org-html-publish-to-html
;;  :headline-levels 3             ; Just the default for this project.
;;  :auto-preamble t
;;  :auto-sitemap t                  ; Generate sitemap.org automagically...
;;  :sitemap-filename "sitemap.org"  ; ... call it sitemap.org (it's the default)...
;;  :sitemap-title "Mapa de sitio"   ; ... with title...
;;  )

;; ("org-static" ; copies images, tables etc to publishing directory towards publication
;;  :base-directory "~/Dropbox/data/elecs/MXelsCalendGovt/reelec/webPage/"
;;  :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
;;  :publishing-directory "~/Dropbox/data/elecs/MXelsCalendGovt/reelec/webPage/publishThis/"
;;  :recursive t
;;  :publishing-function org-publish-attachment
;;  )

;; ("org" :components ("org-notes" "org-static")) ; publish all with single command: M-x org-publish-project RET org RET

;; ; directory structure
;; ~/Dropbox/data/elecs/MXelsCalendGovt/reelec/webPage/
;;   |- css/
;;   |  `- stylesheet.css
;;   |- Emacs
;;   |  |- index.org
;;   |  |- gnus.org
;;   |  |- org.org
;;   |  `- snippets.org
;;   |- img/
;;   |- index.org
;;   `- remember.org

;;       ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END configure org-publish-project to export multifile pages to html in one step ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; org export to beamer
(require 'ox-latex)
(add-to-list 'org-latex-classes
             '("beamer"
               "\\documentclass\[presentation\]\{beamer\}"
               ("\\section\{%s\}" . "\\section*\{%s\}")
               ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
               ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))


;; ; org format timestamps http://endlessparentheses.com/better-time-stamps-in-org-export.html
;; (add-to-list 'org-export-filter-timestamp-functions
;;              #'endless/filter-timestamp)
;; (defun endless/filter-timestamp (trans back _comm)
;;   "Remove <> around time-stamps."
;;   (pcase back
;;     ((or `jekyll `html)
;;      (replace-regexp-in-string "&[lg]t;" "" trans))
;;     (`latex
;;      (replace-regexp-in-string "[<>]" "" trans))))
;; ; still need way to bypass locale date language...
;; (setq-default org-display-custom-times t)
;; (setq org-time-stamp-custom-formats
;;       '("<%d %b %Y>" . "<%d/%m/%y %a %H:%M>"))



;; By an unknown contributor: jumps to matching parenthesis/bracket
(global-set-key "%" 'match-paren)

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeX fold mode in AUCTeX --- looks great but is not working                 ;;
;; see https://www.gnu.org/software/auctex/manual/auctex/Folding.html#Folding ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'LaTeX-mode-hook (lambda ()
                             (TeX-fold-mode 1)))

;; ;; Github Flavored Markdown exporter for Org Mode
;; ;; https://github.com/larstvei/ox-gfm
;; (eval-after-load "org"
;;   '(require 'ox-gfm nil t))


