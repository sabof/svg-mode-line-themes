(let ((style (make-st/theme)))
  (setf
   (st/theme-defs style) (lambda () (es-mt/fr-inset 0.5 0.5))
   (st/theme-background style) 'es-mt/bg-grey1
   (st/theme-default-style style) 'es-mt/grey-default-style
   (st/theme-title-style style) 'es-mt/grey-title-style)
  (pushnew style es-mt/themes))

(setq es-mt/current-theme (car es-mt/themes))

(provide 'es-svg-modeline-themes-themes)
;; es-svg-modeline-themes-themes.el ends here
