;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")
(setq user-full-name "Ritesh Kumar"
      user-mail-address "ritesh@emerald.pink")

;; auth sources used by bots, outlined in documentation: https://magit.vc/manual/ghub.html#Storing-a-Token
;; (setq auth-sources '("~/.authinfo"))
(setq auth-sources '("~/.authinfo.gpg"))

;; no title bar:
(add-to-list 'default-frame-alist '(undecorated . t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IDE CONFIGS -- improving the developer experience ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
(setq
 doom-font (font-spec :family "Fira Code" :size 18 :weight 'regular)
 doom-variable-pitch-font (font-spec :family "Fira Sans" :size 18 :weight 'regular)
 ;; doom-symbol-font (font-spec :family "Symbola" :size 22 :weight 'Regular)
 )

;; (defun my-custom-splash-message ()
;;   "Insert a custom splash message with specific font and size."
;;   (let ((message "Welcome to Doom Emacs!"))
;;     (insert (propertize message 'face '(:height 2.0 :weight bold :family "Fira Sans")))))

(defun get-custom-splash-message ()
  "Insert a centered custom splash message with specific font and size."
  (insert (propertize
           (+doom-dashboard--center +doom-dashboard--width "\n🍀 Let's do great things today 🍀\n")
           'face '(:height 2.0 :weight bold :family "Fira Sans"))))


;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-ir-black)
;; =============== Slash image! ============================
(setq fancy-splash-image (concat doom-user-dir "images/lake_louise_sunrise.png"))
;; Remove default short menu from dashboard
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

;; Prepend custom splash message to dashboard functions
(add-hook! '+doom-dashboard-functions :prepend #'get-custom-splash-message)
;; let frame be full size of current screen:
(add-to-list 'initial-frame-alist '(fullscreen . maximized))


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
;; (setq whitespace-style '(face tabs spaces trailing lines space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark missing-newline-at-eof))

;;;;;;;;;;;;;;;;;;;;;;;
;; ORG MODE CONFIGS! ;;
;;;;;;;;;;;;;;;;;;;;;;;
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; Each sequence of keywords defines its own todo states, this is how we can set custom workflows
;; NOTE: it's good if the keywords are all unique!!
;; this defines a custom workflow of todo states and done states, delimited by |
;;
;; configuration on a per-keyword basis for this.  This is
;; achieved by adding special markers ‘!’ (for a timestamp) or ‘@’ (for a
;; note with timestamp) in parentheses after each keyword
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "|" "CANCELED(c)")))

(setq org-log-done 'note) ;; when a todo is done, records a note along w a timestamp
(setq org-log-redeadline 'note) ;; when a todo is done, records a note along w a timestamp

(setq org-agenda-include-inactive-timestamps t) ;; shows inactive time stamps

;; note: doom has it's own templates, so the var list has to be modded instead
(after! org
  (add-to-list 'org-capture-templates
               '("c"
                 "Clocking templates"
                 ))
  ;; intent: add more context about the currently running task for future reference
  (add-to-list 'org-capture-templates
               '("cn"
                 "Add context to running clock"
                 plain (clock)
                 "+ Note @ %U \n Context \n %i %a"))
  )

;; exporter backend function for collapsible block for source code
(defun my/org-html-collapsible-src-block (src-block contents info)
  "Wrap source blocks in a collapsible HTML container."
  (let ((lang (org-element-property :language src-block)))
    (format "<div class=\"collapsible\">
  <div class=\"collapsible-header\">Show/Hide %s Code</div>
  <div class=\"collapsible-content\">
%s
  </div>
</div>"
            (capitalize (or lang "Source"))
            (org-export-with-backend 'html src-block contents info))))

;; exporter backend function for collapsible block for quotes
(defun my/org-html-collapsible-quote-block (quote-block contents info)
  "Wrap quote blocks in a collapsible HTML container."
  (format "<div class=\"collapsible\">
  <div class=\"collapsible-header\">Show/Hide Quote</div>
  <div class=\"collapsible-content\">
%s
  </div>
</div>"
          (org-export-with-backend 'html quote-block contents info)))


;; exporter backend function for collapsible block for drawers
(defun my/org-html-collapsible-drawer (drawer contents info)
  "Export COLLAPSIBLE drawers as collapsible HTML blocks.
The first non-blank line inside the drawer is used as the header."
  (let* ((drawer-name (org-element-property :drawer-name drawer))
         (is-collapsible (string= (downcase drawer-name) "collapsible")))
    (if is-collapsible
        (let* ((lines (split-string contents "\n"))
               (header nil)
               (body-lines '())
               (found-header nil))
          (dolist (line lines)
            (cond
             ((and (not found-header) (not (string-blank-p line)))
              (setq header line)
              (setq found-header t))
             (found-header
              (push line body-lines))))
          (setq body-lines (nreverse body-lines))
          (format "<div class=\"collapsible\">
  <div class=\"collapsible-header\">%s</div>
  <div class=\"collapsible-content\">
%s
  </div>
</div>"
                  (or header "Show/Hide")
                  (org-trim (mapconcat #'identity body-lines "\n"))))
      ;; Fallback for other drawers
      (org-html-drawer drawer contents info))))

;; Custom headline function that adds anchor links
(defun my/org-html-headline-with-anchor (headline contents info)
  "Add anchor to headline using parent backend."
  (let ((html (org-export-with-backend 'html headline contents info))
        (id (or (org-element-property :CUSTOM_ID headline)
                (org-export-get-reference headline info))))
    (if (string-match "\\(<h[1-9][^>]*>\\)" html)
        (replace-match
         (format "%s<a class=\"anchor\" href=\"#%s\">※</a> "
                 (match-string 1 html) id)
         t t html)
      html)))

(with-eval-after-load 'ox-html
  (org-export-define-derived-backend 'my-html 'html
                                     :translate-alist '((src-block . my/org-html-collapsible-src-block)
                                                        (quote-block . my/org-html-collapsible-quote-block)
                                                        (drawer . my/org-html-collapsible-drawer)
                                                        (headline . my/org-html-headline-with-anchor))))

;; (defun my/org-export-to-html (&optional async subtreep visible-only body-only ext-plist)
;;   "Export current buffer to a collapsible HTML file."
;;   (interactive)
;;   (org-export-to-file 'my-html
;;       (org-export-output-file-name ".html" subtreep)
;;     async subtreep visible-only body-only ext-plist))

(defun my/org-export-to-html (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a collapsible HTML file and open it in the default web browser."
  (interactive)
  (let ((output-file (org-export-to-file 'my-html
                         (org-export-output-file-name ".html" subtreep)
                       async subtreep visible-only body-only ext-plist)))
    (when output-file
      (browse-url (concat "file://" (expand-file-name output-file))))))

;;;;;;;;;;;;;;;;;;;;;;   ;;;;;;;;;;;;;;;;;;;;;;    ;;;;;;;;;;;;;;;;;;;;;;





;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;

;;>> transparency things:
;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;(set-frame-parameter (selected-frame) 'alpha <both>)
(set-frame-parameter (selected-frame) 'alpha '(90))
(add-to-list 'default-frame-alist '(alpha . (90)))


(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(90) '(100)))))
(global-set-key (kbd "C-c t") #'toggle-transparency)

;; Set opacity of emacs frames
(defun opacity (value)
  "Sets the opacity of the frame window. 0=transparent/100=opaque"
  (interactive "nOpacity Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))




;;;;;;;;;;;;;;;
;; OVERRIDES ;;
;;;;;;;;;;;;;;;

;; outlines searchable comments in the code:
(after! hl-todo
  (setq hl-todo-keyword-faces
	'(("TODO"   . "#FF0000")
	  ("FIXME"  . "#FF0000")
	  ("DEBUG"  . "#A020F0")
	  ("GOTCHA" . "#FF4500")
	  ("STUB"   . "#1E90FF")
	  ("DEPRECATED"   . "#00FF00")
          )
        )
  )

(after! magit
  (setq magit-revision-show-gravatars '("^Author: t" . "^Commit: t"))
  (setq magit-diff-refine-hunk 'all)
  )


;; (use-package! latex-preview-pane)
;; (latex-preview-pane-enable)

(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1)
  )


(use-package blamer
  :bind (("s-i" . blamer-show-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 20)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :height 140
                   :italic t)))
  :config
  (global-blamer-mode 0))

;; ox-hugo configurations
(use-package ox-hugo
  :ensure t   ;Auto-install the package from Melpa
  ;; :pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :after ox)


                                        ; my usual places;
(defvar sg-lat 1.334510)
(defvar sg-long 103.721200)
(defvar to-lat 43.653225)
(defvar to-long -79.383186)

;; auto theme-switching based on system by hooking onto system events:
(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'modus-operandi t))
    ('dark (load-theme 'modus-vivendi t))))

(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

;; (use-package! circadian
;;   :ensure t
;;   :config
;;   ;; (setq calendar-latitude sg-lat)
;;   ;; (setq calendar-longitude sg-long)
;;   (setq calendar-latitude sg-lat)
;;   (setq calendar-longitude sg-long)
;;   (setq circadian-themes '((:sunrise . doom-gruvbox-light)
;;                            (:sunset  . doom-monokai-machine)))
;;   (circadian-setup))

;; (add-hook 'circadian-after-load-theme-hook
;;           '(lambda (theme)
;;              ;; Line numbers appearance
;;              (setq linum-format 'linum-format-func)
;;              ;; Cursor
;;              (set-default 'cursor-type 'box)
;;              (set-cursor-color "#F52503")))

;; DOOM Modeline configs
(setq doom-modeline-height 28) ; Set minimum height

(custom-set-faces!
  '(mode-line :family "Fira Code" :height 0.9)
  '(mode-line-inactive :family "Fira Code" :height 0.8))

;;(add-hook! 'doom-modeline-mode-hook
;;  (let ((char-table char-width-table))
;;    (while (setq char-table (char-table-parent char-table)))
;;    (dolist (pair doom-modeline-rhs-icons-alist)
;;      (let ((width 2)  ; <-- tweak this
;;            (chars (cdr pair))
;;            (table (make-char-table nil)))
;;        (dolist (char chars)
;;          (set-char-table-range table char width))
;;        (optimize-char-table table)
;;        (set-char-table-parent table char-table)
;;        (setq char-width-table table)))))

(use-package ob-mermaid
  :after org
  :config
  (setq ob-mermaid-cli-path "/Users/rtshkmr/.nvm/versions/node/v20.13.1/bin/mmdc"))  ; Adjust path as needed

;; extensions to org noter:
(use-package! org-noter
  :after (:any org pdf-view)
  :config
  ;; Your org-noter config here
  (require 'org-noter-pdftools))

(use-package! org-pdftools
  :after org
  :hook (org-mode . org-pdftools-setup-link))

(use-package! org-noter-pdftools
  :after org-noter
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))



(require 'toc-org)
(add-hook 'org-mode-hook 'toc-org-mode)


(elfeed-org)
(setq rmh-elfeed-org-files (list "~/org/rss/elfeed.org"))

(after! projectile
  (setq projectile-project-root-files-bottom-up
        (remove ".git" projectile-project-root-files-bottom-up))
  (setq projectile-auto-discover nil)
  )

;; Modus themes gave me some issues, this allows me to set the bullet list faces correctly, based on light/dark
;; (add-hook 'modus-themes-after-load-theme-hook
;;           (lambda ()
;;             (pcase modus-themes--current-theme
;;               ('modus-operandi
;;                (modus-themes-with-colors
;;                  (custom-set-faces!
;;                    `(org-list-dt :foreground ,blue-intense)
;;                    `(org-list    :foreground ,blue-intense))))
;;               ('modus-vivendi
;;                (modus-themes-with-colors
;;                  (custom-set-faces!
;;                    `(org-list-dt :foreground ,cyan)
;;                    `(org-list    :foreground ,cyan)))))))
