;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Shared between macOS and Arch Linux (GNOME).
;; OS checks use Doom's primitive: (featurep :system 'macos) / (featurep :system 'linux).
;; (IS-MAC / IS-LINUX are obsolete since Doom 2.1.0.)

;; safely spawns emacs server
;; NOTE: `doom-finalize' already does this for graphical sessions; this keeps
;; terminal-only sessions covered too.
(after! server
  (unless (server-running-p)
    (server-start)))


;;;;;;;;;;;;;;;;;;;;
;; Generic Admin  ;;
;;;;;;;;;;;;;;;;;;;;

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Ritesh Kumar"
      user-mail-address "hello@rtshkmr.com")

;; auth sources used by bots, outlined in documentation: https://magit.vc/manual/ghub.html#Storing-a-Token
(setq auth-sources '("~/.authinfo.gpg"))

;; NOTE: projectile settings live in the "projectile: project management"
;; section under Package Configs (the duplicate block that was here is removed).
(after! projectile
  (setq projectile-auto-discover nil
        projectile-indexing-method 'alien
        projectile-sort-order 'recentf
        projectile-enable-caching t))

;;;;;;;;;;;;;;;;;;;;;
;; Doom Aesthetics ;;
;;;;;;;;;;;;;;;;;;;;;

;; =============== Doom Theme ============================
;; Follows the OS light/dark setting.
;; - macOS: emacs-plus exposes `ns-system-appearance' (current value) and
;;   `ns-system-appearance-change-functions' (change hook).
;; - GNOME: the freedesktop settings portal exposes
;;   org.freedesktop.appearance/color-scheme over D-Bus
;;   (0 = no preference, 1 = prefer dark, 2 = prefer light).

(defun my/theme-for (appearance)
  "Return the theme symbol to use for APPEARANCE ('light or 'dark).
Anything else (including nil, i.e. unknown) falls back to the dark theme."
  (pcase appearance
    ('light 'modus-operandi)
    (_      'doom-ir-black)))

(defun my/apply-theme (appearance)
  "Switch to the theme for APPEARANCE, disabling any active themes first.
No-op if that theme is already the only one enabled."
  (let ((theme (my/theme-for appearance)))
    (unless (equal custom-enabled-themes (list theme))
      (mapc #'disable-theme custom-enabled-themes)
      (setq doom-theme theme)
      (load-theme theme t))))

;; ----- macOS: emacs-plus appearance hook -----
(when (featurep :system 'macos)
  (add-hook 'ns-system-appearance-change-functions #'my/apply-theme))

;; ----- GNOME: freedesktop settings portal over D-Bus -----
(when (and (featurep :system 'linux)
           (featurep 'dbusbind)          ; Emacs built with D-Bus support
           (require 'dbus nil t))

  (defconst my/portal-service   "org.freedesktop.portal.Desktop")
  (defconst my/portal-path      "/org/freedesktop/portal/desktop")
  (defconst my/portal-interface "org.freedesktop.portal.Settings")

  (defun my/portal--unwrap (value)
    "Strip the list wrappers that D-Bus variants arrive in."
    (while (consp value)
      (setq value (car value)))
    value)

  (defun my/portal--scheme->appearance (scheme)
    "Map a portal color-scheme value to 'dark or 'light."
    (if (eql (my/portal--unwrap scheme) 1) 'dark 'light))

  (defun my/gnome-appearance ()
    "Return the current GNOME appearance ('light or 'dark) via the settings portal.
Returns 'light if the portal is unreachable."
    (my/portal--scheme->appearance
     (or (ignore-errors
           (dbus-call-method :session my/portal-service my/portal-path my/portal-interface
                             "ReadOne" "org.freedesktop.appearance" "color-scheme"))
         ;; Older portals (< 1.15) only implement the deprecated `Read'.
         (ignore-errors
           (dbus-call-method :session my/portal-service my/portal-path my/portal-interface
                             "Read" "org.freedesktop.appearance" "color-scheme")))))

  (defun my/gnome-appearance-changed-h (namespace key value)
    "Handle the portal's SettingChanged signal."
    (when (and (equal namespace "org.freedesktop.appearance")
               (equal key "color-scheme"))
      (my/apply-theme (my/portal--scheme->appearance value))))

  ;; Register once; unregister first so `doom/reload' doesn't stack handlers.
  (defvar my/gnome-appearance--signal nil
    "D-Bus registration object for the color-scheme listener.")
  (when my/gnome-appearance--signal
    (dbus-unregister-object my/gnome-appearance--signal))
  (setq my/gnome-appearance--signal
        (ignore-errors
          (dbus-register-signal :session my/portal-service my/portal-path my/portal-interface
                                "SettingChanged" #'my/gnome-appearance-changed-h))))

;; ----- Shared entry points -----
(defun my/system-appearance ()
  "Current OS appearance as 'light or 'dark, or nil if it can't be determined."
  (cond ((featurep :system 'macos) (bound-and-true-p ns-system-appearance))
        ((fboundp 'my/gnome-appearance) (my/gnome-appearance))))

(defun my/sync-theme-with-system ()
  "Re-read the OS appearance and apply the matching theme."
  (interactive)
  (my/apply-theme (my/system-appearance)))

;; Initial theme: let Doom load it at the normal point in startup.
(setq doom-theme (my/theme-for (my/system-appearance)))

;; ===== Cursor Color Overrides =====
;; Must be set before the modus theme loads.
(setq modus-operandi-palette-overrides
      '((cursor "#228B22")))    ;; Forest green for light

(setq modus-vivendi-palette-overrides
      '((cursor "#00FF00")))    ;; Bright green for dark (unused while dark = doom-ir-black)

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
;; accept.

(defconst my/mono-family
  (if (featurep :system 'macos) "Iosevka" "Iosevka")
  "Monospace font family for this OS; reused by the splash and modeline faces.")

(let ((size 18))
  (setq doom-font (font-spec :family my/mono-family :size size :weight 'regular)
        doom-variable-pitch-font (font-spec :family "Literata" :size size)))

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; Theme is set from the OS appearance in the "Doom Theme" section above
;; (via `doom-theme'), so no static theme here.
;; (setq doom-theme 'doom-ir-black)


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
;; (setq whitespace-style '(face tabs spaces trailing lines space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark missing-newline-at-eof))

(blink-cursor-mode 1)             ;; Enable cursor blinking
(setq blink-cursor-blinks 0)      ;; Blink forever while idle
(setq blink-cursor-interval 0.5)  ;; Blink interval (in seconds, can adjust)

;; ======================== Frame Opacity ========================

(defconst my/frame-alpha-param
  (if (featurep :system 'macos) 'alpha 'alpha-background)
  "Frame parameter used for opacity.
macOS (NS) honours `alpha', which fades the whole frame including text.
On Linux, `alpha-background' (Emacs 29+, GTK/pgtk/X builds) makes only the
background translucent and keeps text fully opaque.")

(defvar my/frame-opacity--value 80
  "Default frame opacity (0-100).")

(defun my/frame-opacity ()
  "Get the current opacity value (pure query)."
  my/frame-opacity--value)

(defun my/set-frame-opacity! (value)
  "Set opacity to VALUE (clamped to 0-100) and apply it to all frames."
  (setq my/frame-opacity--value (max 0 (min 100 value)))
  (modify-all-frames-parameters
   `((,my/frame-alpha-param . ,my/frame-opacity--value)))
  (message "Opacity: %d%%" my/frame-opacity--value))

(defun my/toggle-frame-opacity! ()
  "Toggle opacity between 80 and 100."
  (interactive)
  (my/set-frame-opacity! (if (= (my/frame-opacity) 100) 80 100)))

(defun my/init-frame-defaults ()
  "Initialise `default-frame-alist': no title bar, configured opacity, fullscreen."
  (setq default-frame-alist
        `((undecorated . t)                                   ;; no title bar
          (,my/frame-alpha-param . ,my/frame-opacity--value)  ;; backtick interpolates
          (fullscreen . fullboth))))

(my/init-frame-defaults)

(defun my/apply-frame-opacity-on-init ()
  "Apply configured opacity to all frames."
  (modify-all-frames-parameters
   `((,my/frame-alpha-param . ,my/frame-opacity--value))))

;; Apply opacity to existing frames on startup and to new server frames.
(add-hook 'doom-init-ui-hook #'my/apply-frame-opacity-on-init)
(add-hook 'server-after-make-frame-hook #'my/apply-frame-opacity-on-init)

(map! :leader
      :desc "Toggle opacity" "t o" #'my/toggle-frame-opacity!)


;; =============== Startup, splash image ============================

(defconst my/splash-message "🍀 Let's do great things today 🍀"
  "Custom splash screen message.")

(defface my/splash-message-face
  '((t (:height 2.0 :weight bold :family "Literata" :slant italic)))
  "Face for custom splash message.")

(defface my/splash-date-face
  `((t (:height 1.8 :weight light :family ,my/mono-family)))
  "Face for splash screen date.")

(defun my/dashboard-insert-date-centered ()
  "Insert centered date using pixel-accurate display properties."
  (let* ((date-str (format-time-string "%A, %d %B %Y"))
         (date-width (string-pixel-width (propertize date-str 'face 'my/splash-date-face)))
         (win-width  (window-pixel-width)))
    (insert (propertize " " 'display `(space :align-to (,(/ (- win-width date-width) 2)))))
    (insert (propertize date-str 'face 'my/splash-date-face))))

(defun my/dashboard-insert-splash ()
  "Insert centered splash message using pixel-accurate display properties."
  (let* ((msg (format "%s" my/splash-message))
         (msg-width  (string-pixel-width (propertize msg 'face 'my/splash-message-face)))
         (win-width  (window-pixel-width)))
    (insert "\n")
    (insert (propertize " " 'display `(space :align-to (,(/ (- win-width msg-width) 2)))))
    (insert (propertize msg 'face 'my/splash-message-face))
    (insert "\n\n")
    (my/dashboard-insert-date-centered)
    (insert "\n\n\n")))

(setq fancy-splash-image (expand-file-name "images/lake_louise_sunrise.png" doom-user-dir))

(remove-hook '+dashboard-functions #'+dashboard-widget-shortmenu)
(add-hook! '+dashboard-functions :prepend #'my/dashboard-insert-splash)

(defun my/dashboard-redraw-on-resize (frame)
  "Redraw dashboard when frame is resized (e.g. on fullscreen)."
  (when (and (boundp '+dashboard-name)
             (get-buffer +dashboard-name)
             (get-buffer-window +dashboard-name frame))
    (+dashboard-reload)))

(add-hook 'window-size-change-functions #'my/dashboard-redraw-on-resize)

(defun my/dashboard-insert-loaded-centered ()
  "Render +dashboard-widget-loaded and insert it pixel-centered."
  (let* ((str (with-temp-buffer
                (+dashboard-widget-loaded)
                (string-trim (buffer-string))))
         (str-width (string-pixel-width str))
         (win-width (window-pixel-width)))
    (insert "\n")
    (insert (propertize " " 'display `(space :align-to (,(/ (- win-width str-width) 2)))))
    (insert str)
    (insert "\n")))

(setq +dashboard-banner-padding '(4 . 2))

;; Explicitly control the order: text → banner → loaded
(setq +dashboard-functions
      (list #'my/dashboard-insert-splash
            #'+dashboard-widget-banner
            #'my/dashboard-insert-loaded-centered))

;; =============== Doom Modeline ============================
;; --- preload:
;; Must be set BEFORE doom-modeline loads
(setq doom-modeline-support-imenu t)
(setq display-time-format "%H:%M:%S"
      display-time-interval 1
      battery-update-interval 30)

(after! doom-modeline
  (setq doom-modeline-height 32
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-minor-modes nil
        doom-modeline-indent-info nil
        doom-modeline-buffer-encoding nil
        doom-modeline-vcs-max-length 30

        ;; Time & Battery
        doom-modeline-battery t
        doom-modeline-time t
        doom-modeline-time-icon nil
        doom-modeline-time-live-icon nil))

;; Defer mode activations and use a sane update interval
(add-hook 'doom-init-ui-hook
          (lambda ()
            (blink-cursor-mode 1)
            (display-time-mode 1)
            (display-battery-mode 1)))

;; Pretty fonts for modeline
(custom-set-faces!
  `(mode-line
    :family ,my/mono-family
    :height 0.95
    :weight semi-bold)
  `(mode-line-inactive
    :family ,my/mono-family
    :height 0.9))


;; =============== Spacious Padding ============================
(use-package! spacious-padding
  :config
  (setq spacious-padding-widths
        '(:internal-border-width 15
          :header-line-width 4
          :mode-line-width 0
          :tab-width 4
          :right-divider-width 30
          :scroll-bar-width 8))
  (spacious-padding-mode 1))

;; =============== OS-specific: macOS ============================
(when (featurep :system 'macos)
  ;; Fullscreen within the current Space instead of a separate native-fullscreen Space.
  (setq ns-use-native-fullscreen nil)

  (defun my/hide-transient-childframes (&rest _)
    "Hide transient childframes (posframe, etc.) without killing them."
    (when (fboundp 'posframe-hide-all)
      (posframe-hide-all))
    (dolist (f (frame-list))
      (when (frame-parameter f 'parent-frame)
        (make-frame-invisible f t))))

  (defun my/after-fullscreen-toggle (&rest _)
    "Hide childframes shortly after a fullscreen toggle."
    ;; Small delay avoids macOS redraw race conditions.
    (run-with-timer 0.01 nil #'my/hide-transient-childframes))

  (advice-add 'toggle-frame-fullscreen :after #'my/after-fullscreen-toggle))

;; =============== OS-specific: Linux (GNOME) ============================
(when (featurep :system 'linux)
  ;; On macOS, Doom's :os macos module binds `SPC o o' to reveal-in-Finder.
  (defun my/reveal-in-file-manager ()
    "Open the current buffer's directory in the system file manager (Nautilus)."
    (interactive)
    (let ((dir (file-name-directory (or (buffer-file-name) default-directory))))
      (call-process "xdg-open" nil 0 nil dir)
      (message "Opened %s in file manager." dir)))

  (map! :leader
        :desc "Reveal in file manager" "o o" #'my/reveal-in-file-manager))

;;;;;;;;;;;;;;
;; Org Mode ;;
;;;;;;;;;;;;;;

;; =============== preloads: ============================
(setq cdlatex-math-symbol-alist nil
      ;; If you use `org' and don't want your org files in the default location below,
      ;; change `org-directory'. It must be set before org loads!
      org-directory "~/org/"

      org-log-done 'note
      org-log-redeadline 'note
      org-agenda-include-inactive-timestamps t)

;; Global keybindings
(map! "C-c l" #'org-store-link
      "C-c a" #'org-agenda
      "C-c c" #'org-capture)

(after! org
  ;; Hooks
  (add-hook 'org-mode-hook #'mixed-pitch-mode)

  ;; babel: defer to after org loads -- this is about lazy registration
  (add-to-list 'org-babel-load-languages '(mermaid . t))
  (add-to-list 'org-babel-load-languages '(ocaml . t))
  (add-to-list 'org-babel-load-languages '(typescript . t))

  ;; Capture templates
  (add-to-list 'org-capture-templates
               '("c" "Clocking templates"))
  (add-to-list 'org-capture-templates
               '("cn"
                 "Add context to running clock"
                 plain (clock)
                 "+ Note @ %U \n Context \n %i %a"))

  ;; Local keymaps (org-mode only)
  (map! :map org-mode-map
        :localleader
        "t" nil
        "tt" #'org-babel-tangle
        "tb" #'org-babel-tangle-single-block))

;; =============== org babel: ============================
(defun my/find-ts-node ()
  "Finds ts-node binary in latest nvm Node version."
  (or (executable-find "ts-node")
      (when-let* ((nvm-dir (expand-file-name "~/.nvm"))
                  ((file-directory-p nvm-dir))
                  (versions (directory-files
                             (expand-file-name "versions/node" nvm-dir)
                             nil "^v" t))
                  (latest (car (sort versions #'string>)))
                  (ts-node (expand-file-name
                            (format "versions/node/%s/bin/ts-node" latest)
                            nvm-dir)))
        (when (file-executable-p ts-node) ts-node))))

(defun my/find-tsx ()
  "Finds tsx binary in latest nvm Node version."
  (or (executable-find "tsx")
      (when-let* ((nvm-dir (expand-file-name "~/.nvm"))
                  ((file-directory-p nvm-dir))
                  (versions (directory-files
                             (expand-file-name "versions/node" nvm-dir)
                             nil "^v" t))
                  (latest (car (sort versions #'string>)))
                  (tsx (expand-file-name
                        (format "versions/node/%s/bin/tsx" latest)
                        nvm-dir)))
        (when (file-executable-p tsx) tsx))))

(defun org-babel-execute:typescript (body params)
  (when-let ((tsx (my/find-tsx)))
    (let* ((tmp-file (make-temp-file "org-babel-ts-" nil ".ts"))
           (result (progn
                     (write-region body nil tmp-file)
                     (shell-command-to-string (format "%s %s" tsx tmp-file)))))
      (delete-file tmp-file)
      (org-babel-result-cond params
        result
        (when (not (string= (org-trim result) ""))
          (split-string result "\n"))))))

(defun my/find-mmdc ()
  "Finds mmdc binary in PATH or latest nvm Node version."
  (or (executable-find "mmdc")
      (when-let* ((nvm-dir (expand-file-name "~/.nvm"))
                  ((file-directory-p nvm-dir))
                  (versions (directory-files
                             (expand-file-name "versions/node" nvm-dir)
                             nil "^v" t))
                  (latest (car (sort versions #'string>)))
                  (mmdc (expand-file-name
                         (format "versions/node/%s/bin/mmdc" latest)
                         nvm-dir)))
        (when (file-executable-p mmdc) mmdc))))
;; (use-package ob-mermaid
;;   :after org
;;   :config
;;   (setq ob-mermaid-cli-path "/Users/rtshkmr/.nvm/versions/node/v23.3.0/bin/mmdc"))  ; Adjust path as needed

(use-package! ob-mermaid
  :after org
  :config
  (when-let ((mmdc (my/find-mmdc)))
    (setq ob-mermaid-cli-path mmdc)))

;; =============== org tooling: ============================
(use-package! pdf-tools
  :defer t)

(use-package! org-noter
  :after org
  :defer t)

(use-package! toc-org
  :after org
  :defer t)

;;;;;;;;;;;;;;;;;;;;;
;; Package Configs ;;
;;;;;;;;;;;;;;;;;;;;;
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

;; =============== Spelling & Dictionaries ============================
(defconst my/ispell-dictionary "en_GB"
  "Default dictionary for ispell/aspell.")


(after! flyspell
  (when (executable-find "aspell")
    (setq ispell-program-name "aspell"
          ispell-dictionary my/ispell-dictionary
          ispell-local-dictionary my/ispell-dictionary)))

;; (when (executable-find "aspell")
;;   (setq ispell-program-name "aspell"
;;         ispell-dictionary my/ispell-dictionary
;;         ispell-local-dictionary my/ispell-dictionary))


;; =============== magit ============================
;; (after! magit
;;   (setq magit-git-editor nil
;;         magit-revision-show-gravatars '("^Author: t" . "^Commit: t")
;;         magit-diff-refine-hunk 'all
;;         ;; Show word-diff in diffs (great for reviewing changes within lines)
;;         magit-diff-options '("--word-diff")
;;         magit-refresh-status-buffer 'auto)  ;; refresh when visible
;;   ;; ===== ENHANCEMENTS =====
;;   magit-save-repository-buffers 'dontask  ;; auto-save before magit ops
;;   magit-display-buffer-function           ;; reuse existing window
;;   #'magit-display-buffer-same-window-except-diff-v1
;;   magit-log-arguments '("-n256" "--graph" "--decorate" "--color")
;;   magit-status-headers-hook               ;; faster status buffer
;;   (remove 'magit-insert-tags-header magit-status-headers-hook))

(after! magit
  ;; ===== Main settings =====
  (setq magit-git-editor nil
        magit-revision-show-gravatars '("^Author: t" . "^Commit: t")
        magit-diff-refine-hunk 'all
        magit-diff-options '("--word-diff")
        magit-refresh-status-buffer 'auto
        magit-save-repository-buffers 'dontask
        magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  ;; ===== Hook customization (separate from setq) =====
  (remove-hook 'magit-status-headers-hook #'magit-insert-tags-header))


(use-package! magit-todos
  :after magit
  :config (magit-todos-mode 1))


;; =============== blamer ============================
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

(use-package! blamer
  ;; NOTE: `s-' is Cmd on macOS; on GNOME it's Super, which the shell may
  ;; intercept before Emacs sees it.
  :bind (("s-i" . blamer-show-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 20)
  :config
  (global-blamer-mode 0))
;; Apply face once initially
;; (my/blamer-face-modus-theme))


;; =============== ox-hugo:blogging system ============================

;; ox-hugo configurations
(use-package! ox-hugo
  :after ox
  :config
  (add-to-list 'org-hugo-special-block-type-properties
               '("mermaid" . (:raw t))))


;; =============== projectile: project management ============================

(after! projectile
  (setq projectile-project-root-files-bottom-up
        (remove ".git" projectile-project-root-files-bottom-up)
        projectile-auto-discover nil
        projectile-indexing-method 'alien       ;; use external tools (fd/rg)
        projectile-sort-order 'recentf          ;; most recent files first
        projectile-enable-caching t))           ;; cache project files


;; =============== elfeed: rss feed ============================
(after! elfeed
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/org/rss/elfeed.org")
        elfeed-search-filter  "@1-week-ago +unread"
        elfeed-db-directory   (expand-file-name "elfeed" doom-data-dir)))

(map! :leader
      (:prefix ("e" . "elfeed")
       :desc "Open elfeed" "e" #'elfeed
       :desc "Update elfeed" "u" #'elfeed-update
       :desc "Export to OPML" "x" #'elfeed-org-export-opml
       :desc "Import from OPML" "i" #'elfeed-org-import-opml))

;; =============== TRAMP ============================
(after! tramp
  ;; TRAMP performance improvements:
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))

  (connection-local-set-profiles
   '(:application tramp :protocol "ssh")
   'remote-direct-async-process)

  (setq tramp-default-method "ssh"
        tramp-verbose 1                         ;; reduce logging overhead
        tramp-auto-save-directory                ;; don't auto-save remotely
        (expand-file-name "tramp-autosave" doom-cache-dir)
        remote-file-name-inhibit-cache nil       ;; don't expire cache
        tramp-use-ssh-controlmaster-options nil)) ;; let ~/.ssh/config handle it

;; =============== nov.el: epub reading ============================
(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-text-width t)  ;; use window width
  (add-hook 'nov-mode-hook
            (lambda ()
              (face-remap-add-relative
               'variable-pitch :family "Literata" :height 1.2)
              (visual-line-mode 1)
              (mixed-pitch-mode 1))))


;;;;;;;;;;;;;;;;;;;;;;
;; Debugger Configs ;;
;;;;;;;;;;;;;;;;;;;;;;

(defvar my/pg-bin (expand-file-name "~/pgsql-dev/bin/"))
(defvar my/pg-port "5433")
(defun my/pg-read-backend-pid ()
  "Prompt for a Postgres backend and return its PID."
  (let* ((cmd (format "%spsql -X -Atc \"select pid||'  '||backend_type||'  '||coalesce(left(query,40),'') from pg_stat_activity where pid <> pg_backend_pid() order by backend_type, pid\" -p %s postgres"
                      my/pg-bin my/pg-port))
         ;; Keep only real "PID  type  query" rows, so a psql error message
         ;; can't be offered as a choice (and parsed as PID 0).
         (rows (seq-filter (lambda (r) (string-match-p "\\`[0-9]+ " r))
                           (split-string (shell-command-to-string cmd) "\n" t))))
    (unless rows
      (user-error "No backends found; is the server up on port %s?" my/pg-port))
    (string-to-number
     (car (split-string (completing-read "Attach to backend: " rows nil t))))))


;; (defconst my/lldb-dap
;;   (or (executable-find "lldb-dap")                              ;; Arch: `lldb' package
;;       "/Library/Developer/CommandLineTools/usr/bin/lldb-dap")   ;; macOS CLT
;;   "Path to the lldb-dap adapter.")

(defun my/lldb-dap-path ()
  "Path to the lldb-dap adapter.
Looks on PATH first (Arch: `lldb' package; Homebrew llvm), then asks `xcrun'
which adapter the active Xcode / Command Line Tools provides (macOS).  Falls
back to the bare name so dape's `ensure' check reports the failure clearly."
  (or (executable-find "lldb-dap")
      (and (featurep :system 'macos)
           (ignore-errors (car (process-lines "xcrun" "-f" "lldb-dap"))))
      "lldb-dap"))



(after! dape
  (add-to-list 'dape-configs
               (if (featurep :system 'macos)
                   `(pg-attach
                     modes (c-mode c-ts-mode)
                     ensure dape-ensure-command
                     command ,(my/lldb-dap-path)
                     command-cwd dape-command-cwd
                     :type "lldb-dap"
                     :request "attach"
                     :pid my/pg-read-backend-pid)
                 `(pg-attach
                   modes (c-mode c-ts-mode)
                   ensure dape-ensure-command
                   command "gdb"
                   ;; Postgres uses SIGUSR1 for latches; don't stop on it.
                   ;; The dap-log pair writes gdb's DAP traffic to /tmp/gdb-dap.log;
                   ;; delete those last two strings once things are stable.
                   command-args ("--interpreter=dap"
                                 "-iex" "handle SIGUSR1 nostop noprint pass"
                                 "-iex" "set debug dap-log-file /tmp/gdb-dap.log")
                   command-cwd dape-command-cwd
                   :request "attach"
                   :pid my/pg-read-backend-pid)))
  ;; Info buffers (scopes, stack, breakpoints, ...) beside the source.
  ;; Try 'gud for a gdb-mi-like grid.
  (setq dape-buffer-window-arrangement 'right
        ;; Side windows span the full frame height.
        window-sides-vertical t)

  ;; Click the fringe to toggle breakpoints in any buffer.
  (dape-breakpoint-global-mode 1)

  ;; Persist breakpoints across sessions.  Registered here, not globally, so a
  ;; session that never loaded dape can't overwrite the saved set.
  ;; NOTE: breakpoints are line-based and can drift as the checkout moves.
  (add-hook 'kill-emacs-hook #'dape-breakpoint-save)
  (dape-breakpoint-load)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language-specific Configs ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; =============== Python ============================

;; =============== OCaml ============================

(after! tuareg
  (let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
    (when (and opam-share (file-directory-p opam-share))
      ;; Registers Merlin
      (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
      ;; auto-starts merlin in OCaml buffers
      (autoload 'merlin-mode "merlin" nil t nil)
      (add-hook 'tuareg-mode-hook 'merlin-mode t)
      (add-hook 'caml-mode-hook 'merlin-mode t)
      ;; Use opam switch to lookup ocamlmerlin binary
      (setq merlin-command 'opam)
      ;; To easily change opam switches within a given Emacs session, you can
      ;; install the minor mode https://github.com/ProofGeneral/opam-switch-mode
      ;; and use one of its "OPSW" menus.
      )))

;; <commented out because I don't want it to hardcode the default switch path>
;;----- ocp-indent
;; (add-to-list 'load-path "/Users/rtshkmr/.opam/default/share/emacs/site-lisp")
;; (require 'ocp-indent)

;;;;;;;;;;;;;;;;;;;
;; LLM-workflows ;;
;;;;;;;;;;;;;;;;;;;

;; =============== key-management: ============================
(defun my/get-claude-api-key ()
  (auth-source-pick-first-password :host "anthropic.com" :user "apikey"))

(defun my/get-mistral-api-key ()
  (auth-source-pick-first-password :host "api.mistral.ai" :user "apikey"))

(defun my/get-moonshot-api-key ()
  (auth-source-pick-first-password :host "api.moonshot.ai" :user "apikey"))


;; =============== gptel config ============================
(use-package! gptel
  :config
  ;; --- UI aspects ---
  (setq gptel-highlight-mode t) ;; show output in highlights
  ;; --- Backend definitions ---
  (setq my/gptel-moonshot
        (gptel-make-openai "Moonshot"
          :host "api.moonshot.ai"
          :key #'my/get-moonshot-api-key
          :stream t
          :models '("kimi-k3" "kimi-k2.7-code" "kimi-k2.6")))

  (setq my/gptel-claude
        (gptel-make-anthropic "Claude"
          :stream t
          :key #'my/get-claude-api-key))

  (setq my/gptel-mistral
        (gptel-make-openai "MistralLeChat"
          :host "api.mistral.ai"
          :endpoint "/v1/chat/completions"
          :protocol "https"
          :key #'my/get-mistral-api-key
          :models '("mistral-small")))

  ;; --- Default model assignment (separate) ---
  (setq gptel-backend my/gptel-moonshot
        gptel-model "kimi-k2.6")

  ;; --- Benchmark / debug infrastructure ---
  (setq gptel-log-level 'debug
        gptel-expert-commands t
        gptel-include-reasoning 'ignore)

  ;; --- Directives: data as separate tuple list ---
  (setq my/gptel-directives
        '((default . "You are a careful, concise software engineer.")
          (audit . "You are a senior auditor. Review the code for:
- Violated invariants and implicit assumptions
- Edge cases that break the contract
- Security or resource-leak risks
Return findings as a structured list with severity (Critical/Warning/Info).")
          (contract . "You are an API designer. Evaluate the contract/interface for:
- Abstraction leaks
- Breaking-change risk under likely evolution
- Missing preconditions or error contracts
Suggest concrete signature or type changes if applicable.")
          (evolve . "You are a refactoring advisor. Given the current implementation, suggest the smallest change that preserves behavior while making the code resilient to the most likely 2-3 future requirements. Explain the trade-off.")
          (synth . "You are a research synthesizer. I will paste code excerpts from a codebase. Summarize how the feature works, identify the key files/functions, and flag any inconsistencies.")))

  ;; --- Keybindings ---
  (map! :leader
        "l l" #'gptel-send
        "l m" #'gptel-menu
        "l c" #'gptel
        "l r" #'gptel-rewrite
        :prefix ("l d" . "gptel directive")
        :desc "Audit"     "a" (cmd! (gptel-send nil (alist-get 'audit my/gptel-directives)))
        :desc "Contract"  "c" (cmd! (gptel-send nil (alist-get 'contract my/gptel-directives)))
        :desc "Evolve"    "e" (cmd! (gptel-send nil (alist-get 'evolve my/gptel-directives)))
        :desc "Synthesize" "s" (cmd! (gptel-send nil (alist-get 'synth my/gptel-directives))))
  )

;; =============== pilish config ============================
;; (use-package! pilish
;;  :init (defalias 'pi 'pilish))



;; ============ IRC ==============
(after! circe
  (setq circe-network-options
        `(("Libera Chat"
           :tls t
           :tls-keylist ((,(expand-file-name "~/.config/irc/libera-key.pem")
                          ,(expand-file-name "~/.config/irc/libera-cert.pem")))
           :sasl-external t
           :nick "rtshkmr"
           :user "rtshkmr"
           :realname "Ritesh Kumar"
           :channels ("#emacs-circe" "#emacs" "#emacs-social"
                      "#postgresql" "#postgresql-lounge"
                      "#python-dev" "#python-infra"))))

  ;; Reconnect behaviour (verify names/defaults with C-h v)
  (setq circe-server-auto-reconnect t
        circe-server-max-reconnect-attempts 20)

  ;; Optional: only track mentions/DMs in noisy channels.
  ;; Each entry is (BUFFER-NAME-OR-REGEXP FACE...): only those faces are tracked.
  ;; (setq tracking-ignored-buffers
  ;;       '(("#python-dev" circe-highlight-nick-face)
  ;;         ("#emacs-social" circe-highlight-nick-face)))

  ;; Mentions outrank ordinary chatter in the modeline
  (setq tracking-faces-priorities '(circe-highlight-nick-face
                                    circe-my-message-face)))

;; ---- Global bindings: reachable from any buffer ----
;; Check `SPC o' in which-key first; this only binds if "o I" is free.
(defun my/leader-key-free-p (keys)
  "Non-nil if KEYS is unbound under Doom's leader map."
  (and (boundp 'doom-leader-map)
       (let ((b (lookup-key doom-leader-map (kbd keys))))
         (or (null b) (numberp b)))))

(if (my/leader-key-free-p "o I")
    (map! :leader
          (:prefix ("o I" . "irc")
           :desc "Next active buffer" "t" #'+irc/tracking-next-buffer
           :desc "Jump to channel"    "c" #'+irc/jump-to-channel
           :desc "Reconnect"          "R" #'circe-reconnect))
  (message "SPC o I already bound; skipping IRC leader group"))

;; ---- Major-mode bindings: SPC m (a c j m n p Q R already taken) ----
(map! :after circe
      :map circe-mode-map
      :localleader
      :desc "Jump to track bar" "b" #'lui-track-jump-to-indicator
      :desc "Whois"             "w" #'circe-command-WHOIS)
