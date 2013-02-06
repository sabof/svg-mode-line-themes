(defvar smt/themes nil)
(defvar smt/current-theme nil)
(defun es-mt/window-width ()
  (let (( window-edges (window-pixel-edges)))
    (- (nth 2 window-edges) (nth 0 window-edges))))

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

(defun es-mt/bg-grey1-top ()
  (let (( width (es-mt/window-width))
        ( height (frame-char-height)))
    `((rect :width "100%" :height 1 :x 0 :y 0 :fill "white" :fill-opacity 0.3)
      (rect :width "100%" :height 1 :x 0 :y ,(- height 2) :fill "black" :fill-opacity 0.2)
      (rect :width "100%" :height 1 :x 0 :y ,(1- height) :fill "black" :fill-opacity 0.6)
      )))

(defun es-mt/bg-black-crystal ()
  (let (( width (es-mt/window-width))
        ( height (frame-char-height)))
    `((\defs
       (linearGradient
        :id "grad1" :x1 "0%" :y1 "0%" :x2 "0%" :y2 "100%"
        (stop :offset "0%" :style "stop-color:#000")
        (stop :offset "50%" :style "stop-color:#222")
        (stop :offset "51%" :style "stop-color:#000")
        (stop :offset "100%" :style "stop-color:#000")
        ))
      (rect :width "100%" :height "100%" :x 0 :y 0 :fill "url(#grad1)")
      (rect :width "100%" :height 1 :x 0 :y 0 :fill "white" :fill-opacity 0.3)
      )))

(defun* smt/fr-inset (&optional (dark-opacity 0.5) (light-opacity 0.5))
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
      )))

(defun smt/mode-indicators ()
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
  ;; Sucky
  (let ((font-size (* 0.7 (es-mt/font-size))))
    (floor
     (+ font-size
        (/ (- (frame-char-height)
              font-size)
           2)))))

(defun es-mt/text-style ()
  `(:font-family
    ,(face-attribute 'default :family)
    :font-size ,(es-mt/font-size)))

(defun smt/+ (plistA plistB)
  (let ((plistC (copy-list plistA))
        key val)
    (dotimes (iter (/ (length plistB) 2))
      (setq key (nth (* 2 iter) plistB)
            val (nth (1+ (* 2 iter)) plistB))
      (if (null val)
          (remf plistC key)
          (setf (getf plistC key) val)))
    plistC))

(defun smt/maybe-funcall (thing)
  (if (or (functionp thing)
          (and (symbolp thing)
               (fboundp thing)))
      (funcall thing)
      thing))

(defun es-mt/grey-default-style ()
  (smt/+
   (es-mt/text-style)
   `(:filter
     "url(#inset)"
     :fill "#b7c3cd"
     )))

(defun es-mt/grey-title-style ()
  (smt/+
   (es-mt/grey-default-style)
   `(:filter
     "url(#inset)"
     :fill ,(if (and (eq (frame-selected-window (selected-frame))
                         (selected-window)))
                "#D4A535"
                "#4C5055")
     ;; :font-family "Georgia, Serif"
     ;; :font-style "italic"
     :y nil
     :font-weight "bold"
     )))

(defun es-mt/text-left ()
  (let (( project-name
          (esprj-project-name
           (esprj-file-project
            (or (buffer-file-name)
                (ignore-errors (dired-current-directory))))))
        )
    (concat
     (if (or (eq system-type 'windows-nt) (daemonp))
         "" "S")
     (when (window-dedicated-p) "D")
     (when buffer-read-only "R")
     (when (mfile-remote-p) " REMOTE")
     " "
     (when project-name (concat project-name " => "))
     (format-mode-line "%b")
     (if (and (or (buffer-file-name)
                  buffer-offer-save)
              (buffer-modified-p))
         "*"))))

(defun smt/diesel-major-mode-style ()
  (smt/+
   (es-mt/text-style)
   `(:fill
     "#ccc"
     :font-family "Georgia, Serif"
     :font-style "italic"
     :font-weight "bold"
     )))

(defun es-mt/default-xml-coverter (theme)
  (assert (smt/theme-p theme))
  (let* (( width (es-mt/window-width))
         ( height (frame-char-height))
         ( text-base-line
           (es-mt/text-base-line))
         ( horizontal-pixel-margin
           (* (smt/theme-margin theme)
              (frame-char-width))))
    (xmlgen
     `(svg
       :xmlns "http://www.w3.org/2000/svg"
       :width ,width
       :height ,height
       ,@(smt/maybe-funcall (smt/theme-defs theme))
       ,@(smt/maybe-funcall (smt/theme-background theme))
       ;; Mode info
       (text :x ,(- width
                    horizontal-pixel-margin
                    (* (frame-char-width) 12))
             :y ,text-base-line
             :text-anchor "end"
             ;; Major-mode
             (tspan
              ,@(smt/maybe-funcall (smt/theme-major-mode-style theme))
              ,(format-mode-line " %m"))
             ;; Version Control
             (tspan
              ,@(smt/maybe-funcall (smt/theme-default-style theme))
              ,(bound-and-true-p vc-mode)
              " ")
             ;; Minor Modes
             (tspan
              ,@(smt/maybe-funcall (smt/theme-minor-mode-style theme))
              ,(smt/maybe-funcall (smt/theme-minor-mode-text theme))))
       ;; Position Info
       (text ,@(smt/maybe-funcall (smt/theme-default-style theme))
             :y ,text-base-line
             :x ,(- width horizontal-pixel-margin)
             :text-anchor "end"
             ,(format-mode-line "%l:%p"))
       ;; Left
       (text
        :x ,horizontal-pixel-margin
        :y ,text-base-line
        :text-anchor "start"
        (tspan
         ,@(smt/maybe-funcall (smt/theme-title-style theme))
         ,(smt/maybe-funcall (smt/theme-buffer-name-text theme))))
       ,@(es-mt/bg-grey1-top)))))

(defstruct smt/theme
  name
  (background (lambda () '(null)))
  (defs (lambda () '(null)))
  (margin 2)
  (title-style 'es-mt/text-style)
  (minor-mode-style 'es-mt/text-style)
  (minor-mode-text 'smt/mode-indicators)
  (default-style 'es-mt/text-style)
  (major-mode-style 'es-mt/text-style)
  (buffer-name-text 'es-mt/text-left)
  (xml-converter 'es-mt/default-xml-coverter))

(defun es-svg-modeline-format ()
  (let* (( image
           (create-image
            (funcall (smt/theme-xml-converter smt/current-theme)
                     smt/current-theme)
            'svg t)))
    (propertize
     "."
     'display image
     'help-echo
     (or (buffer-file-name)
         (ignore-errors
           (dired-current-directory))))))

(defmacro smt/deftheme (name &rest pairs)
  `(let ((theme (make-smt/theme)))
     (setf (smt/theme-name theme) ,name)
     ,@(progn
        (let (result)
          (while pairs
            (push `(setf (,(intern
                            (concat
                             "smt/theme-"
                             (symbol-name (pop pairs))))
                           theme)
                         ,(pop pairs))
                  result))
          (nreverse result)))
     (setq smt/themes
           (acons ,name theme smt/themes))
     (setq smt/current-theme theme)
     ))
(put 'smt/deftheme 'common-lisp-indent-function
     '(1 &body))

(provide 'svg-mode-line-themes-core)
;; svg-mode-line-themes-core.el ends here
