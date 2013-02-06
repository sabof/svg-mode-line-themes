(require 'svg-modeline-themes-core)

(smt/deftheme 'diesel
  :defs (smt/fr-inset 0.5 0.3)
  :background 'es-mt/bg-grey1
  :base-style 'smt/diesel-default-style
  :title-style 'es-mt/grey-title-style
  :minor-mode-style 'es-mt/grey-title-style
  :major-mode-style 'smt/diesel-major-mode-style
  :overlay 'smt/bg-grey1-top)

(smt/deftheme 'nasa
  :background 'smt/bg-nasa
  :base-style (smt/+
               (es-mt/text-style)
               `(:fill "#6D747A"))
  :title-style 'smt/nasa-title-style
  :minor-mode-style 'smt/nasa-title-style
  :major-mode-style 'smt/nasa-major-mode-style
  :overlay 'smt/nasa-overlay)

(defun svg-mode-line-themes-enable ()
  (setq-default mode-line-format
                '(:eval (es-svg-modeline-format))))

(provide 'svg-mode-line-themes)
;; svg-mode-line-themes.el ends here
