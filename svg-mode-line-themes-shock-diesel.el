(require 'svg-mode-line-themes-core)
(require 'svg-mode-line-themes-diesel)

(defun smt/shock-diesel-title-style (widget)
  (smt/combine-styles
   (smt/t-style (smt/t-prototype widget))
   (list :fill (if (smt/window-active-p)
                   "#17C8A2"
                   "#3B2B24")
         :font-weight "bold")))

(smt/deftheme shock-diesel
  :prototype 'diesel
  :background
  (lambda (theme)
    (append
     (smt/t-background (smt/t-prototype theme))
     `((rect :width "100%" :height "100%" :x 0 :y 0
             :fill "#4C1E00" :fill-opacity 0.5))))
  :style
  (lambda (theme)
    (smt/combine-styles
     (smt/t-style 'archetype)
     `(:filter
       "url(#inset)"
       :fill "#3B2B24")))
  :local-widgets
  (lambda (theme)
    (list (cons 'buffer-name
                (smt/make-widget
                 :prototype 'buffer-name
                 :style 'smt/shock-diesel-title-style))
          (cons 'minor-modes
                (smt/make-widget
                 :prototype 'minor-modes
                 :style 'smt/shock-diesel-title-style))
          (cons 'major-mode
                (smt/make-widget
                 :prototype 'major-mode
                 :style
                 (lambda (widget)
                   (smt/combine-styles
                    (smt/diesel-major-mode-style nil)
                    '(:fill "#B8AD96"))))))))

(provide 'svg-mode-line-themes-shock-diesel)
;; svg-mode-line-themes-shock-diesel.el ends here
