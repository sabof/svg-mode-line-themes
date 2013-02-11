(require 'svg-mode-line-themes-core)

(defun smt/black-crystal-title-style ()
  (list ;; :font-weight "bold"
   :fill (if (and (eq (frame-selected-window (selected-frame))
                      (selected-window)))
             "#EE0000"
             "#4C5055")))

(defun smt/bg-black-crystal ()
  `((rect :width "100%" :height "100%" :x 0 :y 0 :fill "#000")))

(defun smt/black-crystal-overlay ()
  (let (( width (smt/window-pixel-width))
        ( height (frame-char-height)))
    `((\defs
       (linearGradient
        :id "grad1" :x1 "0%" :y1 "0%" :x2 "0%" :y2 "100%"
        (stop :offset "0%" :style "stop-color:#fff;stop-opacity:0.0")
        (stop :offset "50%" :style "stop-color:#fff;stop-opacity:0.15")
        (stop :offset "51%" :style "stop-color:#fff;stop-opacity:0.0")
        (stop :offset "100%" :style "stop-color:#fff;stop-opacity:0.05")
        ))
      (rect :width "100%" :height "100%" :x 0 :y 0 :fill "url(#grad1)")
      (rect :width "100%" :height 1 :x 0 :y 0 :fill "white" :fill-opacity 0.2)
      (rect :width "100%" :height 1 :x 0 :y ,(- height 2) :fill "#fff" :fill-opacity 0.2)
      )))

(smt/deftheme black-crystal
  :defs (smt/filter-inset 1 0.3)
  :background 'smt/bg-black-crystal
  :style
  (lambda ()
    (smt/+
     (smt/default-base-style)
     `(:fill "#7E868D")))
  :local-widgets
  (list (cons 'major-mode
              (smt/make-widget
               :parent 'major-mode
               :style 'smt/diesel-major-mode-style))
        (cons 'minor-modes
              (smt/make-widget
               :parent 'minor-modes
               :style 'smt/black-crystal-title-style))
        (cons 'buffer-name
              (smt/make-widget
               :parent 'buffer-name
               :style 'smt/black-crystal-title-style)))
  :rows (list 'default-left 'default-position 'default-right)
  :overlay 'smt/black-crystal-overlay)

(provide 'svg-mode-line-themes-black-crystal)
;; svg-mode-line-themes-black-crystal.el ends here
