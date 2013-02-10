(require 'cl)
(require 'svg-mode-line-themes-core)
(require 'svg-mode-line-themes-styles)
(require 'svg-mode-line-themes-widgets)
(defvar smt/line-format 'mode-line-format)

(setq smt/themes
      (acons 'default (default-value 'mode-line-format)
             smt/themes))

(smt/deftheme nasa
  :defs (smt/filter-inset 0 1)
  :background 'smt/bg-nasa
  :base-style (lambda ()
                (smt/+
                 (smt/default-base-style)
                 `(:filter
                   "url(#inset)"
                   :fill "#404448")))
  ;; :buffer-name-style 'smt/nasa-title-style
  ;; :minor-mode-style 'smt/nasa-title-style
  ;; :major-mode-style 'smt/nasa-major-mode-style
  :overlay 'smt/nasa-overlay)

(smt/deftheme black-crystal
  :defs (smt/filter-inset 1 0.3)
  :background 'smt/bg-black-crystal
  :base-style (lambda ()
                (smt/+
                 (smt/default-base-style)
                 `(:fill "#7E868D")))
  ;; :buffer-name-style 'smt/black-crystal-title-style
  ;; :minor-mode-style 'smt/black-crystal-title-style
  ;; :major-mode-style 'smt/diesel-major-mode-style
  :overlay 'smt/black-crystal-overlay)

(smt/deftheme diesel
  :defs (smt/filter-inset 0.5 0.3)
  :background 'smt/bg-grey1
  :base-style (lambda ()
                (smt/+
                 (smt/default-base-style)
                 `(:filter
                   "url(#inset)"
                   :fill "#b7c3cd")))
  ;; :buffer-name-style 'smt/grey-title-style
  ;; :minor-mode-style 'smt/grey-title-style
  ;; :major-mode-style 'smt/diesel-major-mode-style
  :rows (list 'default-left 'default-position 'default-right)
  :overlay 'smt/bg-grey1-top)

(defun smt/next-theme ()
  (interactive)
  (let* (( position (position smt/current-theme smt/themes :key 'car))
         ( next-theme
           (or (car (nth (1+ position) smt/themes))
               (car (nth 0 smt/themes)))))
    (setq smt/current-theme next-theme)
    ;; (funcall (smt/theme-setup-hook (smt/get-current-theme)))
    (force-mode-line-update)
    (message "Current mode-line theme: %s" next-theme)))

(defun smt/set-theme (theme)
  (interactive
   (list (intern (completing-read
                  "Set mode-line theme to: "
                  (mapcar 'symbol-name (mapcar 'car smt/themes)) nil t))))
  (setq smt/current-theme theme)
  ;; (funcall (smt/theme-setup-hook (smt/get-current-theme)))
  (force-mode-line-update))

(defun smt/enable (&optional use-header-line)
  (set-default (if use-header-line
                   'header-line-format
                   'mode-line-format)
               '(:eval (smt/modeline-format)))
  ;; (funcall (smt/theme-setup-hook (smt/get-current-theme)))
  (force-mode-line-update))

(provide 'svg-mode-line-themes)
;; svg-mode-line-themes.el ends here
