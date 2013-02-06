(require 'svg-modeline-themes-core)

(smt/deftheme 'diesel
  defs (lambda () (smt/fr-inset 0.5 0.5))
  background 'es-mt/bg-grey1
  default-style 'es-mt/grey-default-style
  title-style 'es-mt/grey-title-style
  minor-mode-style 'es-mt/grey-title-style
  major-mode-style 'smt/diesel-major-mode-style)

(smt/deftheme 'nasa
  defs (lambda () (smt/fr-inset 0.5 0.5))
  background 'smt/bg-nasa
  default-style 'es-mt/grey-default-style
  title-style 'es-mt/grey-title-style
  minor-mode-style 'es-mt/grey-title-style
  major-mode-style 'es-mt/grey-mode-style)

(defun svg-mode-line-themes-enable ()
  (setq-default mode-line-format
                '(:eval (es-svg-modeline-format))))

(provide 'svg-mode-line-themes)
;; svg-mode-line-themes.el ends here
