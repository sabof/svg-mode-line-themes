(require 'svg-mode-line-themes-core)

(defun smt/nasa-title-style ()
  (list :font-weight "bold"
        :filter nil
        :fill (if (and (eq (frame-selected-window (selected-frame))
                           (selected-window)))
                  "#2B25E6"
                  "#404347")))

(defun smt/nasa-background ()
  (let (( width (smt/window-pixel-width))
        ( height (frame-char-height)))
    `((\defs
       (filter
        :id "blur"
        (feGaussianBlur
         :stdDeviation "5")
        (feComposite))
       ;; (linearGradient
       ;;  :id "grad1" :x1 "0%" :y1 "0%" :x2 "0%" :y2 "100%"
       ;;  (stop :offset "0%" :style "stop-color:#000;stop-opacity:0.2")
       ;;  (stop :offset "25%" :style "stop-color:#000;stop-opacity:0.0")
       ;;  (stop :offset "75%" :style "stop-color:#000;stop-opacity:0.0")
       ;;  (stop :offset "100%" :style "stop-color:#000;stop-opacity:0.2")
       ;;  )
       (linearGradient
        :id "nasa_center_grad" :x1 "0%" :y1 "0%" :x2 "100%" :y2 "0%"
        (stop :offset "0%" :style "stop-color:#fff;stop-opacity:0.0")
        (stop :offset "50%" :style "stop-color:#fff;stop-opacity:0.5")
        (stop :offset "100%" :style "stop-color:#fff;stop-opacity:0.0")
        ))
      (rect :width "100%" :height "100%" :x 0 :y 0 :fill "#888")
      (rect :width "100%" :height "100%" :x 0 :y 0 :fill "#eee" :filter "url(#blur)")
      ;; (rect :width "100%" :height "100%" :x 0 :y 0 :fill "url(#nasa_center_grad)")
      (rect :width "100%" :height 1 :x 0 :y 0 :fill "black" :fill-opacity 0.3)
      (rect :width "100%" :height 1 :x 0 :y ,(1- height) :fill "black" :fill-opacity 0.3)
      )))

(defun smt/nasa-overlay ()
  (let (( width (smt/window-pixel-width))
        ( height (frame-char-height)))
    `((\defs
       (linearGradient
        :id "grad2" :x1 "0%" :y1 "0%" :x2 "0%" :y2 "100%"
        (stop :offset "0%" :style "stop-color:#fff;stop-opacity:0.6")
        ;; (stop :offset "20%" :style "stop-color:#fff;stop-opacity:0.6")
        ;; (stop :offset "30%" :style "stop-color:#fff;stop-opacity:0.6")
        (stop :offset "66%" :style "stop-color:#888;stop-opacity:0.6")
        (stop :offset "100%" :style "stop-color:#222;stop-opacity:0.5")
        ))
      (rect :width "100%" :height "100%" :x 0 :y 0 :fill "url(#grad2)" )
      )))

(defun smt/nasa-major-mode-style ()
  (smt/+ (smt/diesel-major-mode-style)
         (list :fill "#DC1A0C")))

(smt/deftheme nasa
  :defs (smt/filter-inset 0 1)
  :background 'smt/nasa-background
  :style
  (lambda ()
    (smt/+
     (smt/default-base-style)
     `(:filter
       "url(#inset)"
       :fill "#404448")))
  :local-widgets
  (list (cons 'major-mode
              (smt/make-widget
               :parent 'major-mode
               :style 'smt/nasa-major-mode-style))
        (cons 'minor-modes
              (smt/make-widget
               :parent 'minor-modes
               :style 'smt/nasa-title-style))
        (cons 'buffer-name
              (smt/make-widget
               :parent 'buffer-name
               :style 'smt/nasa-title-style)))
  :rows (list 'default-left 'default-position 'default-right)
  :overlay 'smt/nasa-overlay)

(provide 'svg-mode-line-themes-nasa)
;; svg-mode-line-themes-nasa.el ends here
