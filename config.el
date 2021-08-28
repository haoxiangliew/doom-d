;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Hao Xiang Liew"
      user-mail-address "haoxiangliew@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/haoxiangliew/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; emacs settings

;; load secrets
(defun load-if-exists (f)
  (if (file-exists-p (expand-file-name f))
      (load-file (expand-file-name f))))
(load-if-exists "~/.doom.d/secrets.el")
(setq auth-sources '("~/.authinfo"))

;; integrate system clipboard into emacs
(setq select-enable-clipboard t)

;; set default font
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 12)
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 14)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 12)
      doom-unicode-font (font-spec :family "JetBrainsMono Nerd Font" :size 12)
      doom-serif-font (font-spec :family "CMU Serif" :size 12))

;; switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; prevent emacs from flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; autosave
(setq auto-save-default t)

;; raise undo limit to 80 mb
(setq undo-limit 80000000)

;; more granular undo
(setq evil-want-fine-undo t)

;; display file encoding conditionally
(defun doom-modeline-conditional-buffer-encoding ()
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))
(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

;; scroll wheel speed = scrolling speed (for mx master wheel)
(setq mouse-wheel-progressive-speed nil)

;; Add padding to compensate for rounded corners
(setq-default left-margin-width 1 right-margin-width 1)
(set-window-buffer nil (current-buffer))

;; modules settings

;; doom-dashboard

;; splash image
(setq fancy-splash-image (concat doom-private-dir "home.png"))

;; ascii banner
(setq +doom-dashboard-ascii-banner-fn #'chika-widget-banner)
(defun chika-widget-banner ()
  (let* ((banner
          '("⢸⣿⣿⣿⣿⠃⠄⢀⣴⡾⠃⠄⠄⠄⠄⠄⠈⠺⠟⠛⠛⠛⠛⠻⢿⣿⣿⣿⣿⣶⣤⡀⠄"
            "⢸⣿⣿⣿⡟⢀⣴⣿⡿⠁⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⣸⣿⣿⣿⣿⣿⣿⣿⣷"
            "⢸⣿⣿⠟⣴⣿⡿⡟⡼⢹⣷⢲⡶⣖⣾⣶⢄⠄⠄⠄⠄⠄⢀⣼⣿⢿⣿⣿⣿⣿⣿⣿⣿"
            "⢸⣿⢫⣾⣿⡟⣾⡸⢠⡿⢳⡿⠍⣼⣿⢏⣿⣷⢄⡀⠄⢠⣾⢻⣿⣸⣿⣿⣿⣿⣿⣿⣿"
            "⡿⣡⣿⣿⡟⡼⡁⠁⣰⠂⡾⠉⢨⣿⠃⣿⡿⠍⣾⣟⢤⣿⢇⣿⢇⣿⣿⢿⣿⣿⣿⣿⣿"
            "⣱⣿⣿⡟⡐⣰⣧⡷⣿⣴⣧⣤⣼⣯⢸⡿⠁⣰⠟⢀⣼⠏⣲⠏⢸⣿⡟⣿⣿⣿⣿⣿⣿"
            "⣿⣿⡟⠁⠄⠟⣁⠄⢡⣿⣿⣿⣿⣿⣿⣦⣼⢟⢀⡼⠃⡹⠃⡀⢸⡿⢸⣿⣿⣿⣿⣿⡟"
            "⣿⣿⠃⠄⢀⣾⠋⠓⢰⣿⣿⣿⣿⣿⣿⠿⣿⣿⣾⣅⢔⣕⡇⡇⡼⢁⣿⣿⣿⣿⣿⣿⢣"
            "⣿⡟⠄⠄⣾⣇⠷⣢⣿⣿⣿⣿⣿⣿⣿⣭⣀⡈⠙⢿⣿⣿⡇⡧⢁⣾⣿⣿⣿⣿⣿⢏⣾"
            "⣿⡇⠄⣼⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠟⢻⠇⠄⠄⢿⣿⡇⢡⣾⣿⣿⣿⣿⣿⣏⣼⣿"
            "⣿⣷⢰⣿⣿⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⢰⣧⣀⡄⢀⠘⡿⣰⣿⣿⣿⣿⣿⣿⠟⣼⣿⣿"
            "⢹⣿⢸⣿⣿⠟⠻⢿⣿⣿⣿⣿⣿⣿⣿⣶⣭⣉⣤⣿⢈⣼⣿⣿⣿⣿⣿⣿⠏⣾⣹⣿⣿"
            "⢸⠇⡜⣿⡟⠄⠄⠄⠈⠙⣿⣿⣿⣿⣿⣿⣿⣿⠟⣱⣻⣿⣿⣿⣿⣿⠟⠁⢳⠃⣿⣿⣿"
            "⠄⣰⡗⠹⣿⣄⠄⠄⠄⢀⣿⣿⣿⣿⣿⣿⠟⣅⣥⣿⣿⣿⣿⠿⠋⠄⠄⣾⡌⢠⣿⡿⠃"
            "⠜⠋⢠⣷⢻⣿⣿⣶⣾⣿⣿⣿⣿⠿⣛⣥⣾⣿⠿⠟⠛⠉⠄⠄         "))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat
                 line (make-string (max 0 (- longest-line (length line)))
                                   32)))
               "\n"))
     'face 'doom-dashboard-menu-title)))

;; set home window name
(setq +doom-dashboard-name "*home*")

;; remove default footer and loaded for custom ones
(remove-hook '+doom-dashboard-function '(doom-dashboard-widget-footer
                                         doom-dashboard-widget-loaded))

;; add custom header, loaded, and footer to dashboard
(add-hook! 'doom-dashboard-hook)
(setq +doom-dashboard-functions
      '(doom-dashboard-widget-banner
        doom-dashboard-widget-header
        doom-dashboard-widget-shortmenu
        chika-loaded
        doom-dashboard-haoxiangliew-footer))

;; custom footer
(defun doom-dashboard-haoxiangliew-footer ()
  (insert
   "\n"
   (+doom-dashboard--center
    (- +doom-dashboard--width 2)
    (with-temp-buffer
      (insert-text-button (or (all-the-icons-octicon "octoface" :face 'doom-dashboard-footer-icon :height 1.3 :v-adjust -0.15)
                              (propertize "github" 'face 'doom-dashboard-footer))
                          'action (lambda (_) (browse-url "https://github.com/haoxiangliew/doom-d"))
                          'follow-link t
                          'help-echo "Open haoxiangliew/doom.d github page")
      (buffer-string)))
   "\n"))

;; custom header
(defun doom-dashboard-widget-header ()
  (insert
   "\n"
   (propertize
    (+doom-dashboard--center
     +doom-dashboard--width
     (concat "Hi " (user-full-name) "! Welcome to Chika Emacs!"))
    'face 'doom-dashboard-menu-title)
   "\n\n"))

;; padding between banner and header
(setq +doom-dashboard-banner-padding '(0 . 1))

;; custom loaded
(defun chika-loaded ()
  (insert
   "\n\n"
   (propertize
    (+doom-dashboard--center
     +doom-dashboard--width
     (chika-display-benchmark 'return))
    'face 'doom-dashboard-loaded)
   "\n"))
(defun chika-display-benchmark (&optional return-p)
  (funcall (if return-p #'format #'message)
           "Chika loaded %d packages across %d modules in %.03fs!"
           (- (length load-path) (length (get 'load-path 'initial-value)))
           (if doom-modules (hash-table-count doom-modules) 0)
           (or doom-init-time
               (setq doom-init-time
                     (float-time (time-subtract (current-time) before-init-time))))))

;; custom version
(define-derived-mode +doom-dashboard-mode special-mode
  (format "Chika Fujiwara v%s" doom-version)
  "Major mode for the DOOM dashboard buffer."
  :syntax-table nil
  :abbrev-table nil
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq-local whitespace-style nil)
  (setq-local show-trailing-whitespace nil)
  (setq-local hscroll-margin 0)
  (setq-local tab-width 2)
  ;; Don't scroll to follow cursor
  (setq-local scroll-preserve-screen-position nil)
  (setq-local auto-hscroll-mode nil)
  ;; Line numbers are ugly with large margins
  (setq-local display-line-numbers-type nil)
  (cl-loop for (car . _cdr) in fringe-indicator-alist
           collect (cons car nil) into alist
           finally do (setq-local fringe-indicator-alist alist))
  ;; Ensure point is always on a button
  (add-hook 'post-command-hook #'+doom-dashboard-reposition-point-h nil 'local)
  ;; hl-line produces an ugly cut-off line highlight in the dashboard, so don't
  ;; activate it there (by pretending it's already active).
  (setq-local hl-line-mode t))

;; modules

;; calc
(setq calc-angle-mode 'rad
      calc-symbolic-mode t)

;; evil
(after! evil-escape
  (evil-escape-mode -1))
(after! evil
  (setq evil-ex-substitute-global t))
(setq evil-goggles-enable-record-macro nil)
;; (define-key evil-normal-state-map (kbd "q") (lambda () (interactive) (kill-buffer) (doom/window-maximize-buffer)))

;; elcord-mode
(elcord-mode)
(setq elcord-mode-icon-alist '((+doom-dashboard-mode . "chika_icon")
                               (fundamental-mode . "chika_icon")
                               (c-mode . "c-mode_icon")
                               (c++-mode . "cpp-mode_icon")
                               (clojure-mode . "clojure-mode_icon")
                               (csharp-mode . "csharp-mode_icon")
                               (comint-mode . "comint-mode_icon")
                               (cperl-mode . "cperl-mode_icon")
                               ;; (emacs-lisp-mode . (elcord--editor-icon))
                               (emacs-lisp-mode . "emacs_pen_icon")
                               (enh-ruby-mode . "ruby-mode_icon")
                               (erc-mode . "irc-mode_icon")
                               (eshell-mode . "comint-mode_icon")
                               (forth-mode . "forth-mode_icon")
                               (fsharp-mode . "fsharp-mode_icon")
                               (gdscript-mode . "gdscript-mode_icon")
                               (haskell-mode . "haskell-mode_icon")
                               (haskell-interactive-mode . "haskell-mode_icon")
                               (java-mode . "java-mode_icon")
                               (js-mode . "javascript-mode_icon")
                               (kotlin-mode . "kotlin-mode_icon")
                               (go-mode . "go-mode_icon")
                               (latex-mode . "latex-mode_icon")
                               (lisp-mode . "lisp-mode_icon")
                               (magit-mode . "magit-mode_icon")
                               (markdown-mode . "markdown-mode_icon")
                               (meson-mode . "meson-mode_icon")
                               (nix-mode . "nix-mode_icon")
                               (org-mode . "org-mode_icon")
                               (org-agenda-mode . "org-mode_icon")
                               (racket-mode . "racket-mode_icon")
                               (ruby-mode . "ruby-mode_icon")
                               (rust-mode . "rust-mode_icon")
                               (rustic-mode . "rust-mode_icon")
                               (zig-mode . "zig-mode_icon")
                               ("^slime-.*" . "lisp-mode_icon")
                               ("^sly-.*$" . "lisp-mode_icon")
                               (typescript-mode . "typescript-mode_icon")
                               (vterm-mode . "comint-mode_icon")
                               (php-mode . "php-mode_icon")
                               (python-mode . "python-mode_icon")))
(after! elcord
  (setq elcord-client-id "865374458532462602")
  (setq elcord-editor-icon "emacs_icon")
  (setq elcord-use-major-mode-as-main-icon t))

;; elfeed
(add-hook! 'elfeed-search-mode-hook 'elfeed-update)

(defun elfeed-play-with-mpv ()
  "Play entry link with mpv."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single)))
        (quality-arg "")
        (quality-val (completing-read "Max height resolution (0 for unlimited): " '("0" "480" "720" "1080") nil nil)))
    (setq quality-val (string-to-number quality-val))
    (message "Opening %s with height≤%s with mpv..." (elfeed-entry-link entry) quality-val)
    (when (< 0 quality-val)
      (setq quality-arg (format "--ytdl-format=bestvideo[height<=?%s]+bestaudio/best --sub-auto=fuzzy --ytdl-raw-options=ignore-config=,sub-format=en,write-sub=" quality-val)))
    (start-process "elfeed-mpv" nil "mpv" quality-arg (elfeed-entry-link entry))))

(map! :leader
      :desc "elfeed-play-with-mpv"
      "m v" #'elfeed-play-with-mpv)

(defun elfeed-open-with-eww ()
  "Open in eww with `eww-readable'."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single))))
    (eww  (elfeed-entry-link entry))
    (add-hook 'eww-after-render-hook 'eww-readable nil t)))

(map! :leader
      :desc "elfeed-open-with-eww"
      "m e" #'elfeed-open-with-eww)

(after! elfeed
  (setq elfeed-search-filter "@2-weeks-ago -youtube"))

(setq elfeed-feeds (quote
                    (("https://reddit.com/r/popular.rss" reddit popular)
                     ("https://reddit.com/r/emacs.rss" reddit emacs)
                     ("https://reddit.com/r/linux.rss" reddit linux)
                     ("https://reddit.com/r/nixos.rss" reddit nixos)
                     ("https://reddit.com/r/unixporn.rss" reddit unixporn)
                     ("https://reddit.com/r/virginiatech.rss" reddit virginiatech)
                     ("https://www.phoronix.com/rss.php" news phoronix)
                     ("https://lwn.net/headlines/newrss" news lwn)
                     ("https://weekly.nixos.org/feeds/all.rss.xml" news nixos)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCZaT_X_mc0BI-djXOlfhqWQ" youtube vicenews)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCXuqSBlHAE6Xw-yeJA0Tunw" youtube ltt)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCdBK94H6oZT2Q7l0-b0xmMg" youtube shortcircuit)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCsvn_Po0SmunchJYOWpOxMg" youtube dunkey)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCSCoziKHqjqbox3Fv3Pb4BA" youtube esports)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCID1M0bzAxTChjFO8oEyjyw" youtube donghuap)
                     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCVDepsrgho0zQxMvkik7KUw" youtube corejj))))

;; ivy
(after! ivy
  (add-to-list 'ivy-re-builders-alist '(counsel-projectile-find-file . ivy--regex-plus)))
(setq +ivy-buffer-preview t)

;; flycheck
(setq flycheck-check-syntax-automatically '(save mode-enable))

;; magit
(require 'git-commit)
(after! magit
  (magit-delta-mode +1))

;; company

;; real-time completions (may slow down emacs)
(setq company-minimum-prefix-length 1)
(setq company-idle-delay 0.0)

;; disable company in remote buffers
;; (add-hook 'eshell-mode-hook 'disable-company-remote)
;; (defun disable-company-remote ()
;;   (when (and (fboundp 'company-mode)
;;              (file-remote-p default-directory))
;;     (company-mode -1)))

;; lsp
(setq lsp-ui-sideline-enable nil
      lsp-ui-doc-enable nil
      lsp-enable-symbol-highlighting nil)

;; canvas-emacs
(require 'canvas-utils)
(setq canvas-baseurl "https://canvas.vt.edu")

;; which-key
(after! which-key
  (setq which-key-idle-delay 0.5)
  (setq which-key-allow-multiple-replacements t)
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))

;; yassnippet
(setq yas-triggers-in-field t)

;; rainbow-mode
(add-hook! 'rainbow-mode-hook
  (hl-line-mode (if rainbow-mode -1 +1)))

;; latex
(setq TeX-save-query nil
      TeX-show-compilation t)
(setq +latex-viewers '(pdf-tools evince zathura okular skim sumatrapdf))

;; pdf-tools
(setq pdf-view-midnight-colors '("#839496" . "#002b36"))

;; mu4e
;; intelligently load mu4e location in NixOS
(add-to-list 'load-path (replace-regexp-in-string "[()]" "" (format "%s" (file-expand-wildcards "/nix/store/*-mu-*/share/emacs/site-lisp/mu4e"))))
(setq +mu4e-backend 'offlineimap)
(setq mu4e-context-policy 'ask-if-none
      mu4e-compose-context-policy 'always-ask)
(setq +mu4e-gmail-accounts '(("haoxiangliew@gmail.com" . "/gmail")
                             ("haoxiangliew@vt.edu" . "/vtedu")))
(set-email-account! "gmail"
                    '((mu4e-sent-folder        . "/gmail/Sent Mail")
                      (mu4e-drafts-folder      . "/gmail/Drafts")
                      (mu4e-trash-folder       . "/gmail/Trash")
                      (mu4e-refile-folder      . "/gmail/All Mail")
                      (smtpmail-smtp-user      . "haoxiangliew@gmail.com")
                      (mu4e-compose-signature  . "---\nHao Xiang Liew"))
                    t)
(set-email-account! "vtedu"
                    '((mu4e-sent-folder        . "/vtedu/Sent Mail")
                      (mu4e-drafts-folder      . "/vtedu/Drafts")
                      (mu4e-trash-folder       . "/vtedu/Trash")
                      (mu4e-refile-folder      . "/vtedu/All Mail")
                      (smtpmail-smtp-user      . "haoxiangliew@vt.edu")
                      (mu4e-compose-signature  . "---\nHao Xiang Liew"))
                    t)

;; notmuch
(setq +notmuch-mail-folder "~/mail/")
(setq +notmuch-sync-backend "notmuch new")
(after! notmuch
  (setq notmuch-show-log nil
        notmuch-hello-sections `(notmuch-hello-insert-saved-searches
                                 notmuch-hello-insert-alltags)
        notmuch-message-headers-visible nil))

;; alert
(setq alert-default-style 'libnotify)

;; calfw
(setq cfw:org-overwrite-default-keybinding t)
(map! :leader
      :desc "calfw-org"
      "o C" #'my-open-calendar)
(defun my-open-calendar()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Cyan") ; org-agenda
    (cfw:ical-create-source "gcal" "https://calendar.google.com/calendar/ical/ieee.virginiatech%40gmail.com/public/basic.ics" "Green") ; IEEE @ VT
    (cfw:ical-create-source "canvas" "https://canvas.vt.edu/feeds/calendars/user_B7azceel162srPg4Nw9Ax13hcF0aPcJ57bcriQbK.ics" "Red") ; Canvas
   )))

;; org-agenda
(setq org-agenda-include-deadlines t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-tags-column 100)
(setq org-super-agenda-groups '((:name "Today"
                                 :time-grid t
                                 :date today
                                 :deadline today
                                 :scheduled today
                                 :order 1)
                                (:name "Important"
                                 :priority "A"
                                 :order 2)
                                (:name "Overdue"
                                 :deadline past
                                 :scheduled past
                                 :face error
                                 :order 3)
                                (:name "Homework"
                                 :tag "homework"
                                 :order 5)
                                (:name "Tasks"
                                 :tag "tasks"
                                 :order 6)
                                (:name "Classes"
                                 :tag "classes"
                                 :order 7)))
(org-super-agenda-mode)

;; org-wild-notifier
(use-package org-wild-notifier
  :config
  (org-wild-notifier-mode))

;; file associations
(add-to-list 'auto-mode-alist '("\\.ino\\'" . arduino-mode)) ;; .ino -> arduino
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode)) ;; .h -> c

;; spatial-navigate
(global-set-key (kbd "<M-up>") 'spatial-navigate-backward-vertical-bar)
(global-set-key (kbd "<M-down>") 'spatial-navigate-forward-vertical-bar)
(global-set-key (kbd "<M-left>") 'spatial-navigate-backward-horizontal-bar)
(global-set-key (kbd "<M-right>") 'spatial-navigate-forward-horizontal-bar)
(define-key evil-normal-state-map (kbd "M-k") 'spatial-navigate-backward-vertical-box)
(define-key evil-normal-state-map (kbd "M-j") 'spatial-navigate-forward-vertical-box)
(define-key evil-normal-state-map (kbd "M-h") 'spatial-navigate-backward-horizontal-box)
(define-key evil-normal-state-map (kbd "M-l") 'spatial-navigate-forward-horizontal-box)
(define-key evil-insert-state-map (kbd "M-k") 'spatial-navigate-backward-vertical-bar)
(define-key evil-insert-state-map (kbd "M-j") 'spatial-navigate-forward-vertical-bar)
(define-key evil-insert-state-map (kbd "M-h") 'spatial-navigate-backward-horizontal-bar)
(define-key evil-insert-state-map (kbd "M-l") 'spatial-navigate-forward-horizontal-bar)

;; tramp
(after! tramp
  (setq tramp-default-method "ssh")
  (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash")))
(add-hook 'find-file-hook
          (lambda () (when (file-remote-p default-directory)
                       (setq-local projectile-mode-line "Projectile"))))
