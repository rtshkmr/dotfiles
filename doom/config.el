;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Ritesh Kumar"
      user-mail-address "ritesh@emerald.pink")

;; auth sources used by bots, outlined in documentation: https://magit.vc/manual/ghub.html#Storing-a-Token
(setq auth-sources '("~/.authinfo"))

                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IDE CONFIGS -- improve the developer experience! ;;
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))

;; (setq
;;   doom-font (font-spec :family "Fira Code" :size 13 :weight 'regular)
;;   doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13 :weight 'regular)
;; )

;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-ir-black)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; >> only suggest when you ask manuall:
;; (setq company-idle-delay nil)
;; So that flycheck doesn't automaticaly run all the checks everytime you write a new line.
;; (setq flycheck-check-syntax-automatically '(save mode-enable))
'(flycheck-check-syntax-automatically (quote (save idle-change mode-
                                              enabled)))
'(flycheck-idle-change-delay 4) ;; Set delay based on what suits you the best

(after! lsp
  (setq lsp-auto-configure t)
  )

(setq +format-on-save-enabled-modes
      '(not emacs-lisp-mode  ; elisp's mechanisms are good enough
	sql-mode         ; sqlformat is currently broken
	tex-mode         ; latexindent is broken
        python-mode      ; use autopep8-mode (minor mode) instead
	latex-mode))

(setq-hook! 'python-mode-hook
  +format-with-lsp nil)

;; for python, use autopep8 instead
(use-package py-autopep8
  :hook ((python-mode) . py-autopep8-mode))



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
        (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
        (sequence "|" "CANCELED(c)")))

(setq org-log-done 'note) ;; when a todo is done, records a note along w a timestamp
(setq org-log-redeadline 'note) ;; when a todo is done, records a note along w a timestamp


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

;; =============== Custom Variables ============================
(setq fancy-splash-image (concat doom-user-dir "images/fam_splash_v0 3.jpg"))




;; ======================= Ritesh's Custom Functions ===========================

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
(global-set-key (kbd "C-c t") 'toggle-transparency)

;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))


;; ======================= Ritesh's preferred packages ==========================

;;(use-package! latex-preview-pane)
;;(latex-preview-pane-enable)

;;(use-package! circadian
;;  :config
;;  (setq calendar-latitude 42.653225)
;;  (setq calendar-longitude -80.383186)
;;  (setq circadian-themes '((:sunrise . doom-acario-light)
;;                           (:sunset  . doom-outrun-electric)))
;;  (circadian-setup))


(after! dap-mode
  (setq dap-python-debugger 'debugpy))


(after! magit
  (setq magit-revision-show-gravatars '("^Author: t" . "^Commit: t"))
  (setq magit-diff-refine-hunk 'all)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use these faces to outline searchable comments in code ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(custom-set-faces!
  '(mode-line :family "IBM Plex" :height 0.9)
  '(mode-line-inactive :family "IBM Plex" :height 0.9))
