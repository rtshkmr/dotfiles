;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; safely spawns emacs server
(require 'server)
(unless (server-running-p) (server-start))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IDE CONFIGS -- improving the developer experience ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set no title bar for the window:
(add-to-list 'default-frame-alist '(undecorated . t))

;; =============== Slash image! ============================

(defun get-custom-splash-message ()
  "Insert a centered custom splash message with specific font and size."
  (insert (propertize
           (+doom-dashboard--center +doom-dashboard--width "\n🍀 Let's do great things today 🍀\n")
           'face '(:height 2.0 :weight bold :family "Fira Sans"))))


(setq fancy-splash-image (concat doom-user-dir "images/lake_louise_sunrise.png"))
;; Remove default short menu from dashboard
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

;; Prepend custom splash message to dashboard functions
(add-hook! '+doom-dashboard-functions :prepend #'get-custom-splash-message)
;; let frame be full size of current screen:
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; =========================================================



;; =============== Fonts ============================
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

;; (setq
;;  doom-font (font-spec :family "Fira Code" :size 18 :weight 'regular)
;;  doom-variable-pitch-font (font-spec :family "Fira Sans" :size 18 :weight 'regular)
;;  ;; doom-symbol-font (font-spec :family "Symbola" :size 22 :weight 'Regular)
;; Monospace for code
(setq doom-font (font-spec :family "Iosevka NF" :size 18 :weight 'regular))
;; Proportional for prose/org-mode
(setq doom-variable-pitch-font (font-spec :family "Iosevka NFP" :size 18))


;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-ir-black)


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
;; (setq whitespace-style '(face tabs spaces trailing lines space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark missing-newline-at-eof))

;; =========================================================



;; =============== ORG MODE ============================
;;;;;;;;;;;;;;;;;;;;;;;
;; ORG MODE CONFIGS! ;;
;;;;;;;;;;;;;;;;;;;;;;;
(setq cdlatex-math-symbol-alist nil)
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

;; ===== org-babel tangle keymaps (only for org mode)
(map! :map org-mode-map
      :localleader
      "t" nil  ;; clear the existing "t" prefix
      "tt" #'org-babel-tangle
      "tb" #'org-babel-tangle-single-block)

;; %%%%%%%%%%% ORG EXPORTER CUSTOM BACKEND FUNCTIONS %%%%%%%%%%%%%
;; the functions below are my own exporter overrides for the creation of
;; html files from org files.

;; TODO: add this part in, the current export is pretty annoying and it applies for ALL languages.
;; - also see if we can use a js based syntax highlighter instead of using htmlize
;; when htmlize runs, it picks up the actual view from the buffer, so indent guides (with | chars) are copied over.
;; this function should ignore the indent guides for the exporting.

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


(defun my/org-export-to-html (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a collapsible HTML file and open it in the default web browser."
  (interactive)
  (let ((output-file (org-export-to-file 'my-html
                         (org-export-output-file-name ".html" subtreep)
                       async subtreep visible-only body-only ext-plist)))
    (when output-file
      (browse-url (concat "file://" (expand-file-name output-file))))))

;;;;;;;;;;;;;;;;;;;;;;   ;;;;;;;;;;;;;;;;;;;;;;    ;;;;;;;;;;;;;;;;;;;;;;
;; =========================================================
;; ======================== Frame Opacity ========================

(defvar my/frame-opacity 80
  "Default frame opacity (0-100).")

;; Seed default-frame-alist at load time so the first emacsclient frame inherits it
(push `(alpha . ,my/frame-opacity) default-frame-alist)

(defun my/apply-opacity ()
  "Apply opacity to all current and future frames."
  (modify-all-frames-parameters `((alpha . ,my/frame-opacity))))

(add-hook 'doom-init-ui-hook #'my/apply-opacity)
(add-hook 'server-after-make-frame-hook #'my/apply-opacity)

(defun my/toggle-transparency ()
  "Toggle between 100% and 90% opacity."
  (interactive)
  (setq my/frame-opacity (if (= my/frame-opacity 100) 90 100))
  (my/apply-opacity)
  (message "Opacity: %d%%" my/frame-opacity))

(defun my/set-opacity (value)
  "Set opacity to VALUE (0-100)."
  (interactive "nOpacity (0-100): ")
  (setq my/frame-opacity (max 0 (min 100 value)))
  (my/apply-opacity)
  (message "Opacity set to %d%%" my/frame-opacity))

(global-set-key (kbd "C-c t") #'my/toggle-transparency)

;; ================================================================

;; ;; ======================== Frame Opacity ========================

;; (defvar my/frame-opacity 80
;;   "Default frame opacity (0-100).")

;; (defun my/apply-opacity ()
;;   "Apply opacity to all current and future frames."
;;   (modify-all-frames-parameters `((alpha . ,my/frame-opacity))))

;; (add-hook 'doom-init-ui-hook #'my/apply-opacity)
;; (add-hook 'server-after-make-frame-hook #'my/apply-opacity)

;; (defun my/toggle-transparency ()
;;   "Toggle between 100% and 90% opacity."
;;   (interactive)
;;   (setq my/frame-opacity (if (= my/frame-opacity 100) 90 100))
;;   (my/apply-opacity)
;;   (message "Opacity: %d%%" my/frame-opacity))

;; (defun my/set-opacity (value)
;;   "Set opacity to VALUE (0-100)."
;;   (interactive "nOpacity (0-100): ")
;;   (setq my/frame-opacity (max 0 (min 100 value)))
;;   (my/apply-opacity)
;;   (message "Opacity set to %d%%" my/frame-opacity))

;; (global-set-key (kbd "C-c t") #'my/toggle-transparency)

;; ;; ================================================================


;; =============== Package Configs ============================
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

;; outlines searchable comments in the code, for the modus themes and specific to light/dark theme
(after! hl-todo
  (defun my-hl-todo-set-keyword-faces (&rest _)
    (setq hl-todo-keyword-faces
          (if (member (car custom-enabled-themes) '(modus-operandi modus-operandi-tinted))
              ;; Light themes
              `(("TODO" . ,(modus-themes-get-color-value 'red-intense))
                ("FIXME" . ,(modus-themes-get-color-value 'red-intense))
                ("HACK" . ,(modus-themes-get-color-value 'yellow-intense))
                ("BUG" . ,(modus-themes-get-color-value 'red-intense))
                ("XXX" . ,(modus-themes-get-color-value 'magenta-intense))
                ("STUB" . ,(modus-themes-get-color-value 'blue-intense))
                ("NOTE" . ,(modus-themes-get-color-value 'blue-cooler))
                ("REVIEW" . ,(modus-themes-get-color-value 'yellow-intense))
                ("OPTIMIZE" . ,(modus-themes-get-color-value 'green-intense))
                ("DEPRECATED" . ,(modus-themes-get-color-value 'cyan-intense))
                ("TEMP" . ,(modus-themes-get-color-value 'yellow-faint))
                ("HOLD" . ,(modus-themes-get-color-value 'fg-faint))
                ("DONE" . ,(modus-themes-get-color-value 'green-cooler)))
            ;; Dark themes
            `(("TODO" . ,(modus-themes-get-color-value 'red-intense))
              ("FIXME" . ,(modus-themes-get-color-value 'red-intense))
              ("HACK" . ,(modus-themes-get-color-value 'yellow-intense))
              ("BUG" . ,(modus-themes-get-color-value 'red-intense))
              ("XXX" . ,(modus-themes-get-color-value 'magenta-intense))
              ("STUB" . ,(modus-themes-get-color-value 'blue-intense))
              ("NOTE" . ,(modus-themes-get-color-value 'blue-cooler))
              ("REVIEW" . ,(modus-themes-get-color-value 'yellow-intense))
              ("OPTIMIZE" . ,(modus-themes-get-color-value 'green-intense))
              ("DEPRECATED" . ,(modus-themes-get-color-value 'cyan-intense))
              ("TEMP" . ,(modus-themes-get-color-value 'yellow-faint))
              ("HOLD" . ,(modus-themes-get-color-value 'fg-faint))
              ("DONE" . ,(modus-themes-get-color-value 'green-cooler)))))
    (font-lock-flush)
    (font-lock-ensure))
  (add-hook 'modus-themes-after-load-theme-hook #'my-hl-todo-set-keyword-faces))

(setq ispell-program-name "aspell")
(setq ispell-dictionary "en_GB")
(setq ispell-local-dictionary "en_GB")


(blink-cursor-mode 1)             ;; Enable cursor blinking
(setq blink-cursor-blinks 0)      ;; Blink forever while idle
(setq blink-cursor-interval 0.5)  ;; Blink interval (in seconds, can adjust)

;; For Modus Operandi (light)
(setq modus-operandi-palette-overrides
      '((cursor "#228B22")))    ;; Forest green

;; For Modus Vivendi (dark)
(setq modus-vivendi-palette-overrides
      '((cursor "#00FF00")))    ;; Bright green


(after! magit
  (setq magit-git-editor nil)
  (setq magit-revision-show-gravatars '("^Author: t" . "^Commit: t"))
  (setq magit-diff-refine-hunk 'all)
  ;; Show word-diff in diffs (great for reviewing changes within lines)
  (setq magit-diff-options '("--word-diff"))
  ;; Automatically refresh status buffer after performing actions
  (setq magit-refresh-status-buffer nil)
  )

(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1)
  )

;; (use-package! latex-preview-pane)
;; (latex-preview-pane-enable)

;;(use-package blamer
;;  :bind (("s-i" . blamer-show-commit-info))
;;  :defer 20
;;  :custom
;;  (blamer-idle-time 0.3)
;;  (blamer-min-offset 20)
;;  :custom-face
;;  (blamer-face
;;   ;; Use `modus-themes-with-colors` to get palette-dependent foreground color
;;   (modus-themes-with-colors
;;     (let ((fg (if (eq (car custom-enabled-themes) 'modus-operandi)
;;                   modus-themes-blue-alt   ;; light theme blue
;;                 modus-themes-magenta-alt))) ;; dark theme magenta
;;       `((t :foreground ,fg
;;          :background nil
;;          :height 140
;;          :italic t)))))
;;  :config
;;  (global-blamer-mode 0))

(defun my/blamer-face-modus-theme ()
  (modus-themes-with-colors
    (let* ((theme (car custom-enabled-themes))
           (fg (cond
                ((memq theme '(modus-operandi modus-operandi-tinted modus-operandi-deuteranopia))
                 magenta-cooler)  ;; light theme color
                ((memq theme '(modus-vivendi modus-vivendi-tinted modus-vivendi-deuteranopia))
                 magenta)         ;; dark theme color
                (t magenta))))     ;; fallback
      (custom-set-faces
       `(blamer-face ((,c :foreground ,fg
                          :background nil
                          :height 140
                          :italic t)))))))

(add-hook 'modus-themes-after-load-theme-hook #'my/blamer-face-modus-theme)

(use-package blamer
  :bind (("s-i" . blamer-show-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 20)
  :config
  (global-blamer-mode 0)
  ;; Apply face once initially
  (my/blamer-face-modus-theme))



;; ox-hugo configurations
(use-package ox-hugo
  :ensure t   ;Auto-install the package from Melpa
  ;; :pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :after ox)


                                        ; my usual places;


;; auto theme-switching based on system by hooking onto system events:
(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'modus-operandi-tinted t))
    ('dark (load-theme 'modus-vivendi t))))

(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

;; Set cursor color to bright green for all Modus themes
(after! modus-themes
  (set-cursor-color "#00FF00")) ;; Bright green hex code

;; Enable blinking cursor
(blink-cursor-mode 1)

;; Optionally adjust blink timing (in seconds)
(setq blink-cursor-interval 0.5) ;; half a second

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
  (setq ob-mermaid-cli-path "/Users/rtshkmr/.nvm/versions/node/v23.3.0/bin/mmdc"))  ; Adjust path as needed

;; Org Babel Explicit Whitelist:
(org-babel-do-load-languages
 'org-babel-load-languages
 '((mermaid . t)
   (python . t)
   (ocaml . t)))


;; extensions to org noter:
;; (use-package! org-noter
;;   :after (:any org pdf-view)
;;   :config
;;   ;; Your org-noter config here
;;   (require 'org-noter-pdftools)

;;   )

;; (use-package! org-pdftools
;;   :after org
;;   :hook (org-mode . org-pdftools-setup-link))

;; (use-package! org-noter-pdftools
;;   :after org-noter
;;   :config
;;   (with-eval-after-load 'pdf-annot
;;     (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(use-package! pdf-tools
  :defer t)

(use-package! org-noter
  :after org
  :defer t)

;; (use-package! org-noter-pdftools
;;   :after (org-noter pdf-tools)
;;   :config
;; Only activate AFTER epdfinfo exists
;;   (when (and (boundp 'pdf-info-epdfinfo-program)
;;              pdf-info-epdfinfo-program
;;              (file-executable-p pdf-info-epdfinfo-program))
;;     (org-noter-pdftools-mode 1)))



(require 'toc-org)
(add-hook 'org-mode-hook 'toc-org-mode)


(elfeed-org)
(setq rmh-elfeed-org-files (list "~/org/rss/elfeed.org"))

(after! projectile
  (setq projectile-project-root-files-bottom-up
        (remove ".git" projectile-project-root-files-bottom-up))
  (setq projectile-auto-discover nil)
  )

;; === special ocaml setup:
(let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)

    ))
;; Use opam switch to lookup ocamlmerlin binary
(setq merlin-command 'opam)

;;----- ocp-indent
(add-to-list 'load-path "/Users/rtshkmr/.opam/default/share/emacs/site-lisp")
(require 'ocp-indent)

(setq spacious-padding-widths
      '( :internal-border-width 15
         :header-line-width 4
         :mode-line-width 6
         :tab-width 4
         :right-divider-width 30
         :scroll-bar-width 8))
(spacious-padding-mode 1)


;; %%%% TRAMP improvements %%%%%%
(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 '(:application tramp :protocol "ssh")
 'remote-direct-async-process)


;; %%%%-------------------- LLM Configs ------------------------ %%%%%%
;; Mistral offers an OpenAI compatible API
(gptel-make-openai "MistralLeChat"  ;Any name you want
  :host "api.mistral.ai"
  :endpoint "/v1/chat/completions"
  :protocol "https"
  :key <KEY>               ;can be a function that returns the key
  :models '("mistral-small"))

;; OPTIONAL configuration
(setq gptel-model   'mistral-small
      gptel-backend
      (gptel-make-openai "MistralLeChat"  ;Any name you want
        :host "api.mistral.ai"
        :endpoint "/v1/chat/completions"
        :protocol "https"
        :key <KEY>               ;can be a function that returns the key
        :models '("mistral-small")))
