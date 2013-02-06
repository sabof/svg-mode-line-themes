(require 'cl)

(defun es-mt/grey-title-style ()
  `(:fill ,(if (and (eq (frame-selected-window (selected-frame))
                        (selected-window)))
               "#D4A535"
               "#4C5055")
          :font-weight "bold"))

(defun smt/nasa-title-style ()
  (list :font-weight "bold"
        :fill (if (and (eq (frame-selected-window (selected-frame))
                           (selected-window)))
                  "#4A68D4"
                  "#70767D")))

(defun smt/black-crystal-title-style ()
  (list ;; :font-weight "bold"
   :fill (if (and (eq (frame-selected-window (selected-frame))
                      (selected-window)))
             "#EE0000"
             "#4C5055")))

(defun es-mt/bg-grey1 ()
  (let (( width (es-mt/window-width))
        ( height (frame-char-height)))
    `((\defs
       (linearGradient
        :id "grad1" :x1 "0%" :y1 "0%" :x2 "0%" :y2 "100%"
        (stop :offset "0%" :style "stop-color:rgb(255,255,255);stop-opacity:0.1")
        (stop :offset "100%" :style "stop-color:rgb(0,0,0);stop-opacity:0.1"))
       (linearGradient
        :id "grad2" :x1 "0%" :y1 "0%" :x2 "100%" :y2 "0%"
        (stop :offset "0%" :style "stop-color:rgb(255,255,255);stop-opacity:0.0")
        (stop :offset "50%" :style "stop-color:rgb(255,255,255);stop-opacity:0.2")
        (stop :offset "100%" :style "stop-color:rgb(255,255,255);stop-opacity:0.0")))
      (rect :width "100%" :height "100%" :x 0 :y 0 :fill "#ccc" :fill-opacity 0.3)
      (rect :width "100%" :height "100%" :x 0 :y 0 :fill "url(#grad1)")
      (rect :width "100%" :height "100%" :x 0 :y 0 :fill "url(#grad2)")
      )))

(defun smt/bg-grey1-top ()
  (let (( width (es-mt/window-width))
        ( height (frame-char-height)))
    `((rect :width "100%" :height 1 :x 0 :y 0 :fill "white" :fill-opacity 0.3)
      (rect :width "100%" :height 1 :x 0 :y ,(- height 2) :fill "black" :fill-opacity 0.2)
      (rect :width "100%" :height 1 :x 0 :y ,(1- height) :fill "black" :fill-opacity 0.6)
      )))

(defun smt/bg-black-crystal ()
  `((rect :width "100%" :height "100%" :x 0 :y 0 :fill "#000")))

(defun smt/black-crystal-overlay ()
  (let (( width (es-mt/window-width))
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

(defun* smt/filter-inset (&optional (dark-opacity 0.5) (light-opacity 0.5))
  `((filter
     :id "inset"
     (feOffset :in "sourceGraphic" :dx -1 :dy -1 :result "o_dark")
     (feOffset :in "sourceGraphic" :dx 2 :dy 2 :result "o_light")
     ;; http://www.w3.org/TR/SVG/filters.html#feColorMatrixElement
     ;; http://en.wikipedia.org/wiki/Matrix_multiplication#Illustration
     (feColorMatrix
      :type "matrix"
      :in "o_light" :result "o_light"
      :values ,(concat
                "0  0  0  0  1 "
                "0  0  0  0  1 "
                "0  0  0  0  1 "
                (format
                 "0  0  0  %s  0 "
                 light-opacity)
                ))
     (feColorMatrix
      :type "matrix"
      :in "o_dark" :result "o_dark"
      :values ,(concat
                "0  0  0  0  -1 "
                "0  0  0  0  -1 "
                "0  0  0  0  -1 "
                (format
                 "0  0  0  %s  0 "
                 dark-opacity)
                ))
     (feMerge
      (feMergeNode :in "o_dark")
      (feMergeNode :in "o_light")
      (feMergeNode :in "SourceGraphic")
      ))))

(defun smt/bg-nasa ()
  (let (( width (es-mt/window-width))
        ( height (frame-char-height)))
    `((\defs
       (filter
        :id "blur"
        (feGaussianBlur
         :stdDeviation "5")
        (feComposite))
       (linearGradient
        :id "grad1" :x1 "0%" :y1 "0%" :x2 "0%" :y2 "100%"
        (stop :offset "0%" :style "stop-color:#000;stop-opacity:0.2")
        (stop :offset "25%" :style "stop-color:#000;stop-opacity:0.0")
        (stop :offset "75%" :style "stop-color:#000;stop-opacity:0.0")
        (stop :offset "100%" :style "stop-color:#000;stop-opacity:0.2")
        ))
      (rect :width "100%" :height "100%" :x 0 :y 0 :fill "#888")
      (rect :width "100%" :height "100%" :x 0 :y 0 :fill "white" :filter "url(#blur)")
      (rect :width "100%" :height 1 :x 0 :y ,(1- height) :fill "black" :fill-opacity 0.1)
      )))

(defun smt/nasa-overlay ()
  (let (( width (es-mt/window-width))
        ( height (frame-char-height)))
    `((\defs
       (linearGradient
        :id "grad2" :x1 "0%" :y1 "0%" :x2 "0%" :y2 "100%"
        (stop :offset "0%" :style "stop-color:#fff;stop-opacity:0.3")
        (stop :offset "100%" :style "stop-color:#000;stop-opacity:0.5")
        ))
      (rect :width "100%" :height "100%" :x 0 :y 0 :fill "url(#grad2)" )
      )))

(defun smt/diesel-major-mode-style ()
  `(:fill
    "#ccc"
    :font-family "Georgia, Serif"
    :font-style "italic"
    :filter nil
    :font-weight "bold"
    ))

(defun smt/nasa-major-mode-style ()
  (smt/+ (smt/diesel-major-mode-style)
         (list :fill "#B62208")))

(provide 'svg-mode-line-themes-styles)
;; svg-mode-line-themes-styles.el ends here
