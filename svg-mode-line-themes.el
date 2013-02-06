(require 'svg-modeline-themes-core)
(require 'svg-modeline-themes-styles)

(smt/deftheme 'nasa
  :background 'smt/bg-nasa
  :base-style (smt/+
               (es-mt/default-base-style)
               `(:fill "#6D747A"))
  :title-style 'smt/nasa-title-style
  :minor-mode-style 'smt/nasa-title-style
  :major-mode-style 'smt/nasa-major-mode-style
  :overlay 'smt/nasa-overlay)

(smt/deftheme 'diesel
  :defs (smt/fr-inset 0.5 0.3)
  :background 'es-mt/bg-grey1
  :base-style (smt/+
               (es-mt/default-base-style)
               `(:filter
                 "url(#inset)"
                 :fill "#b7c3cd"))
  :title-style 'es-mt/grey-title-style
  :minor-mode-style 'es-mt/grey-title-style
  :major-mode-style 'smt/diesel-major-mode-style
  :overlay 'smt/bg-grey1-top)

(defun svg-mode-line-themes-enable ()
  (setq-default mode-line-format
                '(:eval (es-svg-modeline-format))))

(provide 'svg-mode-line-themes)
;; svg-mode-line-themes.el ends here
