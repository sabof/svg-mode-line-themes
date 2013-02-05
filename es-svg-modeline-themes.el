(defun es-mt/window-width ()
  (let (( window-edges (window-pixel-edges)))
    (- (nth 2 window-edges) (nth 0 window-edges))))

(defvar es-mt/bg-grey1
  (let (( width (es-mt/window-width))
        ( height (frame-char-height)))
    (xmlgen
     `(g
       (\defs
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
       (rect :width "100%" :height 1 :x 0 :y 0 :fill "white" :fill-opacity 0.3)
       (rect :width "100%" :height 1 :x 0 :y ,(1- height) :fill "black" :fill-opacity 0.1)
       ))))

(defvar es-mt/bg-black-crystal
  (let (( width (es-mt/window-width))
        ( height (frame-char-height)))
    (xmlgen
     `(g
       (\defs
        (linearGradient
         :id "grad1" :x1 "0%" :y1 "0%" :x2 "0%" :y2 "100%"
         (stop :offset "0%" :style "stop-color:#000")
         (stop :offset "50%" :style "stop-color:#222")
         (stop :offset "51%" :style "stop-color:#000")
         (stop :offset "100%" :style "stop-color:#000")
         ))
       (rect :width "100%" :height "100%" :x 0 :y 0 :fill "url(#grad1)")
       (rect :width "100%" :height 1 :x 0 :y 0 :fill "white" :fill-opacity 0.3)
       ))))

(defvar es-mt/fr-inset
  (xmlgen
   `(filter
     :id "inset"
     (feOffset :in "sourceGraphic" :dx -1 :dy -1 :result "o_up")
     (feOffset :in "sourceGraphic" :dx 2 :dy 2 :result "o_down")
     ;; http://www.w3.org/TR/SVG/filters.html#feColorMatrixElement
     ;; http://en.wikipedia.org/wiki/Matrix_multiplication#Illustration
     (feColorMatrix
      :type "matrix"
      :in "o_down" :result "o_down2"
      :values (concat
               "0  0  0  0  1 "
               "0  0  0  0  1 "
               "0  0  0  0  1 "
               "0  0  0  0  0 "))
     (feColorMatrix
      :type "matrix"
      :in "o_up" :result "o_up2"
      :values (concat
               "0  0  0  0  -1 "
               "0  0  0  0  -1 "
               "0  0  0  0  -1 "
               "0  0  0  0  0 "))
     (feMerge
      (feMergeNode :in "o_up2")
      (feMergeNode :in "SourceGraphic")
      ))))

(defvar es-mt/bg-nasa
  (let (( width (es-mt/window-width))
        ( height (frame-char-height)))
    (xmlgen
     `(g
       (\defs
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
         )
        (linearGradient
         :id "grad2" :x1 "0%" :y1 "0%" :x2 "0%" :y2 "100%"
         (stop :offset "0%" :style "stop-color:#000;stop-opacity:0.0")
         (stop :offset "100%" :style "stop-color:#000;stop-opacity:0.3")
         ))
       (rect :width "100%" :height "100%" :x 0 :y 0 :fill "#888")
       (rect :width "100%" :height "100%" :x 0 :y 0 :fill "white" :filter "url(#blur)")
       ;; (rect :width "100%" :height "100%" :x 0 :y 0 :fill "url(#grad1)")
       (rect :width "100%" :height "100%" :x 0 :y 0 :fill "url(#grad2)")
       ;; (rect :width "100%" :height "100%" :x 0 :y 0 :fill "url(#grad1)")
       ;; (rect :width "100%" :height 1 :x 0 :y 1 :fill "white" :fill-opacity 1)
       (rect :width "100%" :height 1 :x 0 :y ,(1- height) :fill "black" :fill-opacity 0.1)
       ))))

(defun es-mt/mode-indicators ()
  (concat
   (when (bound-and-true-p es-aai-mode) "I")
   (when (or (bound-and-true-p evil-local-mode)
             (bound-and-true-p evil-mode)) "E")
   (when truncate-lines "T")
   (when dired-omit-mode "O")
   (when (bound-and-true-p save-auto-hook) "A")
   (when (bound-and-true-p wmi) "M")))

(defun es-mt/font-size ()
  (- (frame-char-height) 4))

(defun es-mt/text-base-line ()
  (let ((font-size (* (es-mt/font-size) 0.7)))
    (+ font-size
       (/ (- (frame-char-height) font-size) 2))))

(defun es-svg-modeline-theme1 ()
  (xmlgen
   (let* (( window-edges (window-pixel-edges))
          ( width (- (nth 2 window-edges) (nth 0 window-edges)))
          ( height (frame-char-height))
          ( horizontal-margin 20)
          ( default-font-family (face-attribute 'default :family))
          ( default-font-color (face-attribute 'default :foreground))
          ( mode-font-family "Georgia, Serif")
          ( title-color
            (if (and (eq (frame-selected-window (selected-frame))
                         (selected-window)))
                "#ccad42"
                "#888"))
          ( font-size (es-mt/font-size))
          ( text-base-line (es-mt/text-base-line))
          ( project-name
            (esprj-project-name
             (esprj-file-project
              (or (buffer-file-name)
                  (ignore-errors (dired-current-directory)))))))
     `(svg
       :xmlns "http://www.w3.org/2000/svg"
       :width ,width
       :height ,height
       ;; :version "1.1"
       (!escape
        ,es-mt/bg-grey1
        ,es-mt/fr-inset)
       (g :font-family ,default-font-family
          :fill ,default-font-color
          :font-size ,font-size
          (text :x ,(- width horizontal-margin (* (frame-char-width) 12))
                :y ,text-base-line
                :text-anchor "end"
                :style "filter:url(#inset)"
                (tspan
                 :font-family ,mode-font-family
                 :font-weight "bold"
                 :font-style "italic"
                 ,(format-mode-line " %m"))
                ,(bound-and-true-p vc-mode)
                " "
                (tspan
                 :fill ,title-color
                 :font-weight "bold"
                 ,(es-mt/mode-indicators)))
          (text :x ,(- width horizontal-margin)
                :y ,text-base-line
                :text-anchor "end"
                :style "filter:url(#inset)"
                ,(format-mode-line "%l:%p"))
          (text :x ,horizontal-margin
                :y ,text-base-line
                :text-anchor "start"
                :font-weight "bold"
                :fill ,title-color
                :style "filter:url(#inset)"
                ,(concat
                  (if (or (eq system-type 'windows-nt) (daemonp))
                      "" "S")
                  (when (window-dedicated-p) "D")
                  (when buffer-read-only "R")
                  (when (mfile-remote-p) " REMOTE")
                  " "
                  (when project-name (concat project-name " => "))
                  (format-mode-line "%b")
                  (if (and buffer-offer-save (buffer-modified-p))
                      "*"))))))))


(defun es-svg-modeline-format ()
  (let* ((image (create-image (es-svg-modeline-theme1) 'svg t)))
    (propertize "."
                'display image
                'help-echo (or (buffer-file-name)
                               (ignore-errors (dired-current-directory))))))

(defun es-svg-modeline-set ()
  (interactive)
  (setq-default header-line-format
                '(:eval (es-svg-modeline-format)))
  (set-face-attribute
   'header-line nil
   :box 'unspecified))

(provide 'es-svg-modeline-themes)
;; es-svg-modeline-themes.el ends here
