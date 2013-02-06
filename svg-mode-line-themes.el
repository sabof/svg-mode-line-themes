(require 'cl)
(require 'svg-mode-line-themes-core)
(require 'svg-mode-line-themes-styles)

(smt/deftheme nasa
  :background 'smt/bg-nasa
  :base-style (smt/+
               (es-mt/default-base-style)
               `(:fill "#5D6368"))
  :buffer-name-style 'smt/nasa-title-style
  :minor-mode-style 'smt/nasa-title-style
  :major-mode-style 'smt/nasa-major-mode-style
  :overlay 'smt/nasa-overlay)

(smt/deftheme diesel
  :defs (smt/filter-inset 0.5 0.3)
  :background 'es-mt/bg-grey1
  :base-style (smt/+
               (es-mt/default-base-style)
               `(:filter
                 "url(#inset)"
                 :fill "#b7c3cd"))
  :buffer-name-style 'es-mt/grey-title-style
  :minor-mode-style 'es-mt/grey-title-style
  :major-mode-style 'smt/diesel-major-mode-style
  :overlay 'smt/bg-grey1-top)

(smt/deftheme black-crystal
  :defs (smt/filter-inset 1 0.3)
  :background 'smt/bg-black-crystal
  :base-style (smt/+
               (es-mt/default-base-style)
               `(;; :filter
                 ;; "url(#inset)"
                 :fill "#7E868D"))
  :buffer-name-style 'smt/black-crystal-title-style
  :minor-mode-style 'smt/black-crystal-title-style
  :major-mode-style 'smt/diesel-major-mode-style
  :overlay 'smt/black-crystal-overlay)

(defun smt/next-theme ()
  (interactive)
  (let* ((position (position smt/current-theme smt/themes :key 'car))
         ( next-theme
           (or (car (nth (1+ position) smt/themes))
               (car (nth 0 smt/themes)))))
    (setq smt/current-theme next-theme)
    (force-mode-line-update)
    (when (called-interactively-p 'interactive)
      (message "Current mode-line theme: %s" next-theme))))

(defun smt/set-theme (theme)
  (interactive
   (list (intern (completing-read
                  "Set mode-line theme to: "
                  (mapcar 'symbol-name (mapcar 'car smt/themes)) nil t))))
  (setq smt/current-theme theme)
  (force-mode-line-update))

(defun smt/enable ()
  (setq-default mode-line-format
                '(:eval (es-svg-modeline-format)))
  (force-mode-line-update))

(provide 'svg-mode-line-themes)
;; svg-mode-line-themes.el ends here
