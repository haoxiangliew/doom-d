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
(setq doom-theme 'doom-one)

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

;; integrate system clipboard into emacs
(setq select-enable-clipboard t)

;; set default font
(setq doom-font (font-spec :family "CaskaydiaCove Nerd Font" :size 12)
      doom-big-font (font-spec :family "CaskaydiaCove Nerd Font" :size 14)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 12)
      doom-serif-font (font-spec :family "Times New Roman" :size 12))

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

;; add padding to prevent interference with rounded corners
(set-window-margins (selected-window) 20 20)
(add-hook! '+popup-buffer-mode-hook
  (set-window-margins (selected-window) 20 20))

;; prevent emacs from flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; add arrow key keymaps
(map! :map evil-window-map
      "SPC" #'rotate-layout
      ;; navigation
      "<left>"    #'evil-window-left
      "<right>"   #'evil-window-right
      "<up>"      #'evil-window-up
      "<down>"    #'evil-window-down
      ;; swap windows
      "C-<left>"    #'+evil/window-move-left
      "C-<right>"   #'+evil/window-move-right
      "C-<up>"      #'+evil/window-move-up
      "C-<down>"    #'+evil/window-move-down)

;; emacs 27 ligatures
(use-package composite
  :defer t
  :init
  (defvar composition-ligature-table (make-char-table nil))
  :hook
  (((prog-mode conf-mode nxml-mode markdown-mode help-mode)
    . (lambda () (setq-local composition-function-table composition-ligature-table))))
  :config
  ;; support ligatures, some toned down to prevent hang
  (when (version<= "27.0" emacs-version)
    (let ((alist
           '((33 . ".\\(?:\\(==\\|[!=]\\)[!=]?\\)")
             (35 . ".\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
             (36 . ".\\(?:\\(>\\)>?\\)")
             (37 . ".\\(?:\\(%\\)%?\\)")
             (38 . ".\\(?:\\(&\\)&?\\)")
             (42 . ".\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
             ;; (42 . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
             (43 . ".\\(?:\\([>]\\)>?\\)")
             ;; (43 . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
             (45 . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
             ;; (46 . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
             (46 . ".\\(?:\\(\\.<\\|[-=]\\)[-<=]?\\)")
             (47 . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
             ;; (47 . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
             (48 . ".\\(?:\\(x[a-fA-F0-9]\\).?\\)")
             (58 . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
             (59 . ".\\(?:\\(;\\);?\\)")
             (60 . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
             (61 . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
             (62 . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
             (63 . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
             (91 . ".\\(?:\\(|\\)[]|]?\\)")
             ;; (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
             (94 . ".\\(?:\\(=\\)=?\\)")
             (95 . ".\\(?:\\(|_\\|[_]\\)_?\\)")
             (119 . ".\\(?:\\(ww\\)w?\\)")
             (123 . ".\\(?:\\(|\\)[|}]?\\)")
             (124 . ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
             (126 . ".\\(?:\\(~>\\|[-=>@~]\\)[-=>@~]?\\)"))))
      (dolist (char-regexp alist)
        (set-char-table-range composition-ligature-table (car char-regexp)
                              `([,(cdr char-regexp) 0 font-shape-gstring]))))
    (set-char-table-parent composition-ligature-table composition-function-table))
  )

;; modules settings

;; doom-dashboard
(setq fancy-splash-image (concat doom-private-dir "home.png"))
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
     'face 'doom-dashboard-menu-desc)))
(setq +doom-dashboard-name "*home*")
(remove-hook '+doom-dashboard-function '(doom-dashboard-widget-footer
                                         doom-dashboard-widget-loaded))
(add-hook! 'doom-dashboard-hook)
(setq +doom-dashboard-functions
      '(doom-dashboard-widget-banner
        doom-dashboard-widget-header
        doom-dashboard-widget-shortmenu
        chika-loaded
        doom-dashboard-haoxiangliew-footer))
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
(defun doom-dashboard-widget-header ()
  (insert
   "\n"
   (propertize
    (+doom-dashboard--center
     +doom-dashboard--width
     (concat "Hi " (user-full-name) "! Welcome to Emacs!"))
    'face 'doom-dashboard-menu-desc)
   "\n\n"))
(setq +doom-dashboard-banner-padding '(0 . 1))
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
           (- (length load-path) (length doom--initial-load-path))
           (if doom-modules (hash-table-count doom-modules) 0)
           (or doom-init-time
               (setq doom-init-time
                     (float-time (time-subtract (current-time) before-init-time))))))
(define-derived-mode +doom-dashboard-mode special-mode
  (format "Chika v%s" doom-version)
  :syntax-table nil
  :abbrev-table nil
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq-local whitespace-style nil)
  (setq-local show-trailing-whitespace nil)
  (setq-local hscroll-margin 0)
  (setq-local tab-width 2)
  (setq-local scroll-preserve-screen-position nil)
  (setq-local auto-hscroll-mode nil)
  (setq-local display-line-numbers-type nil)
  (cl-loop for (car . _cdr) in fringe-indicator-alist
           collect (cons car nil) into alist
           finally do (setq fringe-indicator-alist alist))
  (add-hook 'post-command-hook #'+doom-dashboard-reposition-point-h nil t))

;; calc
(setq calc-angle-mode 'rad
      calc-symbolic-mode t)

;; evil
(after! evil-escape
  (evil-escape-mode -1))
(after! evil
  (setq evil-ex-substitute-global t))
(setq evil-goggles-enable-record-macro nil)
(define-key evil-normal-state-map (kbd "q") (lambda () (interactive) (kill-buffer) (doom/window-maximize-buffer)))

;; elcord-mode
(elcord-mode)
(after! elcord
  (setq elcord-use-major-mode-as-main-icon t)
  )

;; ivy
(after! ivy
  (add-to-list 'ivy-re-builders-alist '(counsel-projectile-find-file . ivy--regex-plus)))
(setq +ivy-buffer-preview t)

;; flycheck
(setq flycheck-check-syntax-automatically '(save mode-enable))

;; lsp
(setq lsp-log-io nil)

;; magit
(after! magit
  (magit-delta-mode +1))

;; company-mode
(global-company-mode 1)

(setq company-minimum-prefix-length 1)
(setq company-tooltip-limit 10)
(setq company-idle-delay 0.0)
;; disable company in remote buffers
(add-hook 'eshell-mode-hook 'disable-company-remote)

(defun disable-company-remote ()
  (when (and (fboundp 'company-mode)
             (file-remote-p default-directory))
    (company-mode -1)))

;; tree-sitter
(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

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
      TeX-show-compilation t
      TeX-command-extra-options "-shell-escape")
(after! latex
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t)))
(setq +latex-viewers '(pdf-tools evince zathura okular skim sumatrapdf))

;; mu4e
(setq mu4e-maildir (expand-file-name "~/mbsync"))

(set-email-account! "gmail"
                    '((mu4e-sent-folder        . "/gmail[Gmail].Sent Mail")
                      (mu4e-drafts-folder      . "/gmail[Gmail].Drafts")
                      (mu4e-trash-folder       . "/gmail[Gmail].Trash")
                      (mu4e-refile-folder      . "/gmail[Gmail].All Mail")
                      (smtpmail-smtp-user      . "haoxiangliew@gmail.com")
                      (mu4e-compose-signature  . "---\nHao Xiang Liew"))
                    t)

(set-email-account! "vtedu"
                    '((mu4e-sent-folder        . "/vtedu[vt.edu].Sent Mail")
                      (mu4e-drafts-folder      . "/vtedu[vt.edu].Drafts")
                      (mu4e-trash-folder       . "/vtedu[vt.edu].Trash")
                      (mu4e-refile-folder      . "/vtedu[vt.edu].All Mail")
                      (smtpmail-smtp-user      . "haoxiangliew@vt.edu")
                      (mu4e-compose-signature  . "---\nHao Xiang Liew"))
                    t)

;; calfw
(setq cfw:org-overwrite-default-keybinding t)
(map! :leader
      :desc "calfw-org"
      "o C" #'cfw:open-org-calendar)

;; org
(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
(defadvice! org-edit-latex-emv-after-insert ()
  :after #'org-cdlatex-environment-indent
  (org-edit-latex-environment))

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

;; nov.el
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; vterm
(defcustom vterm-eval-cmds '(("find-file" find-file)
                             ("message" message)
                             ("vterm-clear-scrollback" vterm-clear-scrollback)
                             ("magit-status" magit-status)
                             ("magit-commit" magit-commit)
                             ("magit-stage-file" magit-stage-file)
                             ("magit-stage-modified" magit-stage-modified)
                             ("magit-push" magit-push)
                             ("sudo-find-file" doom/sudo-find-file)
                             ("org-agenda" org-agenda-list)
                             ("mu4e" =mu4e))
  "Whitelisted Emacs functions that can be executed from vterm.
You can execute Emacs functions directly from vterm buffers.  To do this,
you have to escape the name of the function and its arguments with \e]51;E.
See Message passing in README.
The function you want to execute has to be in `vterm-eval-cmds'.
`vterm-eval-cmds' has to be a list of pairs of the format:
\(NAME-OF-COMMAND-IN-SHELL EMACS-FUNCTION)
The need for an explicit map is to avoid arbitrary code execution."
  :type '(alist :key-type string)
  :group 'vterm)

;; tramp
(after! tramp
  (setenv "SHELL" "/bin/bash")
  (setq tramp-default-method "ssh")
  (setq tramp-shell-prompt-pattern "\\(?:^\\|\\)[^]#$%>\n]*#?[]#$%>] *\\(\\[[0-9;]*[a-zA-Z] *\\)*")) ;; default + 
