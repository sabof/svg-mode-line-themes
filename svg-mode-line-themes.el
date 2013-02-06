(require 'svg-modeline-themes-core)
  "Change to header-line-format, if you want to theme headers.")
(let ((style (make-smt/theme)))
  (setf
   (smt/theme-defs style) (es-mt/fr-inset 0.5 0.5)
   (smt/theme-background style) 'es-mt/bg-grey1
   (smt/theme-default-style style) 'es-mt/grey-default-style
   (smt/theme-title-style style) 'es-mt/grey-title-style
   (smt/theme-minor-mode-style style) 'es-mt/grey-title-style
   )
  (pushnew style smt/themes))

(setq smt/current-theme (car smt/themes))

(defun svg-mode-line-themes-enable ()
  (setq-default
   mode-line-format
   '(:eval (es-svg-modeline-format))))

(provide 'svg-mode-line-themes)
;; svg-mode-line-themes.el ends here
