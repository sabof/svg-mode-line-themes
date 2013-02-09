(require 'cl)
(or (require 'xmlgen nil t)
    (require 'xmlgen "xml-gen"))

(defvar smt/themes nil)
(defvar smt/widgets nil)
(defvar smt/rows nil)
(defvar smt/current-theme nil)

;;; Structs

(defstruct (smt/theme
             (:conc-name smt/t-))
  background
  overlay
  defs
  (position-width 12)
  (export-func 'smt/t-export-default)
  (setup-hook 'ignore)
  (base-style 'smt/default-base-style)
  local-widgets
  rows)

(defstruct (smt/row
             (:conc-name smt/r-))
  (alignment 'left)
  (priority 0)
  (width-func 'smt/r-width-default)
  (margin 2)
  widgets
  base-style
  (export-func 'smt/r-export-default))

(defstruct (smt/widget
             (:conc-name smt/w-))
  (style 'smt/default-base-style)
  (on-click 'ignore)
  (text "")
  (width-func 'smt/w-width-default)
  (export-func 'smt/w-export-default))

;;; Structs EOF
;;; Method

(defun smt/t-export-default (theme)
  (let* (( width (smt/window-width))
         ( height (frame-char-height))
         ( rows (smt/t-rows theme))
         xml image)
    (setq
     xml
     (xmlgen
      `(svg
        :xmlns "http://www.w3.org/2000/svg"
        :width ,width
        :height ,height
        ,@(smt/maybe-funcall (smt/t-defs theme))
        ,@(smt/maybe-funcall (smt/t-background theme))
        ,@(mapcar (lambda (row-or-name)
                    (smt/r-export
                     (cond ( (smt/row-p row-or-name)
                             row-or-name)
                           ( (smt/row-p (cdr (assoc row-or-name smt/rows)))
                             (cdr (assoc row-or-name smt/rows)))
                           ( t (error "Row has wrong type: %s" row-or-name)))
                     theme))
                  (smt/t-rows theme))
        ,@(smt/maybe-funcall (smt/t-overlay theme))
        )))
    (setq
     image
     (create-image xml 'svg t))
    (propertize
     "."
     'display image
     'help-echo
     (or (buffer-file-name)
         (ignore-errors
           (dired-current-directory))))))

(defun smt/t-export (theme)
  (funcall (smt/t-export-func theme) theme))

(defun smt/r-export-default (row theme)
  `(text
    :text-anchor ,(case
                   (smt/r-alignment row)
                   (left "start")
                   (right "end")
                   (center "middle"))
    :x ,(case
         (smt/r-alignment row)
         (left (* (smt/r-margin row)
                  (frame-char-width)))
         (right (- (frame-pixel-width)
                   (* (smt/r-margin row)
                      (frame-char-width))))
         (center (/ (frame-pixel-width) 2)))
    :y ,(smt/text-base-line)
    ,@(mapcar (lambda (widget-or-name)
                (smt/w-export
                 (if (smt/widget-p widget-or-name)
                     widget-or-name
                     (cdr (assoc widget-or-name smt/widgets)))
                 row theme))
              (smt/r-widgets row))))

(defun smt/r-export (row theme)
  (funcall (smt/r-export-func row) row theme))

(defun smt/w-export-default (widget row theme)
  `(tspan
    ,@(smt/+ (smt/maybe-funcall (smt/t-base-style theme))
             (smt/maybe-funcall (smt/r-base-style row))
             (smt/maybe-funcall (smt/w-style widget)))
    ,(smt/maybe-funcall (smt/w-text widget))))

(defun smt/w-export (widget row theme)
  (funcall (smt/w-export-func widget) widget row theme))

(defun smt/w-width-default (widget)
  (length (smt/w-text widget)))

;;; Method EOF
;;; Legacy

(defun smt/left-text-width (theme)
  (+ (smt/t-margin theme)
     (length
      (concat
       (smt/maybe-funcall
        (smt/t-buffer-name-text theme))
       (smt/maybe-funcall
        (smt/t-buffer-indicators-text theme))))))

(defun smt/right-text-width (theme)
  (+ (smt/t-margin theme)
     (smt/t-position-width theme)
     (length
      (concat
       (smt/maybe-funcall
        (smt/t-major-mode-text theme))
       (smt/maybe-funcall
        (smt/t-minor-mode-text theme))
       (smt/maybe-funcall
        (smt/t-vc-text theme))))
     ;; Variable-width text safety-margin
     6))

(defun smt/default-xml-coverter (theme)
  (assert (smt/t-p theme))
  (let* (( width (smt/window-width))
         ( height (frame-char-height))
         ( text-base-line
           (smt/text-base-line))
         ( horizontal-pixel-margin
           (* (smt/t-margin theme)
              (frame-char-width)))
         ( left-width
           (funcall (smt/t-left-text-width theme)
                    theme))
         ( right-width
           (funcall (smt/t-right-text-width theme)
                    theme))
         ( position-width
           (smt/maybe-funcall
            (smt/t-position-width theme)))
         ( char-width
           (let ((edges (window-edges)))
             (- (nth 2 edges) (nth 0 edges))))
         ( width-mode
           (cond ( (>= char-width
                       (+ left-width right-width))
                   3)
                 ( (>= char-width
                       (+ left-width position-width))
                   2)
                 (t 1))))
    (xmlgen
     `(svg
       :xmlns "http://www.w3.org/2000/svg"
       :width ,width
       :height ,height
       ,@(smt/maybe-funcall (smt/t-defs theme))
       ,@(smt/maybe-funcall (smt/t-background theme))
       ;; Mode info
       ,@(when
          (> width-mode 2)
          `((text :x ,(- width
                         horizontal-pixel-margin
                         (* (frame-char-width)
                            position-width)
                         0.5)
                  :y ,text-base-line
                  :text-anchor "end"
                  ;; Major-mode
                  (tspan
                   ,@(smt/get-style theme :major-mode-style)
                   " "
                   ,(smt/maybe-funcall (smt/t-major-mode-text theme)))
                  ;; Version Control
                  (tspan
                   ,@(smt/get-style theme :vc-style)
                   ,(smt/maybe-funcall (smt/t-vc-text theme))
                   " ")
                  ;; Minor Modes
                  (tspan
                   ,@(smt/get-style theme :minor-mode-style)
                   ,(smt/maybe-funcall (smt/t-minor-mode-text theme))))))
       ;; Position Info
       ,@(when
          (> width-mode 1)
          `((text ,@(smt/get-style theme :position-style)
                  :x ,(- width horizontal-pixel-margin)
                  :y ,text-base-line
                  :text-anchor "end"
                  ,(format-mode-line "%l:%p"))))
       ;; Left
       (text
        :x ,horizontal-pixel-margin
        :y ,text-base-line
        :text-anchor "start"
        (tspan
         ,@(smt/get-style theme :buffer-indicators-style)
         ,(smt/maybe-funcall (smt/t-buffer-indicators-text theme)))
        (tspan
         ,@(smt/get-style theme :buffer-name-style)
         ,(smt/maybe-funcall (smt/t-buffer-name-text theme))))
       ,@(smt/maybe-funcall (smt/t-overlay theme))))))

;;; Legacy EOF

(defun smt/window-width ()
  (let (( window-edges (window-pixel-edges)))
    (- (nth 2 window-edges) (nth 0 window-edges))))

(defun smt/points-to-pixels (points)
  ;; points = pixels * 72 / 96
  (/ (* 96 points) 72))

(defun smt/font-pixel-size ()
  (round
   (smt/points-to-pixels
    (/ (face-attribute 'default :height) 10)))
  ;; (- (frame-char-height) 4)
  )

(defun smt/text-base-line ()
  ;; Sucky
  (let ((font-size (* 0.7 (smt/font-pixel-size))))
    (floor
     (+ font-size
        (/ (- (frame-char-height)
              font-size)
           2)))))
(defun smt/points-to-pixels (points)
  ;; points = pixels * 72 / 96
  ;;  = pixels * 72
  (/ (* 96 points) 72))

(defun smt/default-base-style ()
  `(:font-family
    ,(face-attribute 'default :family)
    :font-size
    ,(concat (int-to-string
              (round
               (/ (face-attribute 'default :height)
                  10.0)))
             "pt")))

(defun smt/+ (&rest plists)
  (cond
    ( (= 1 (length plists))
      (car plists))
    ( (null plists)
      nil)
    ( t (let ((plistC (copy-list (car plists)))
              (plistB (cadr plists))
              key val)
          (dotimes (iter (/ (length plistB) 2))
            (setq key (nth (* 2 iter) plistB)
                  val (nth (1+ (* 2 iter)) plistB))
            (if (null val)
                (remf plistC key)
                (setf (getf plistC key) val)))
          (apply 'smt/+ plistC (cddr plists))
          ))))

(defun smt/maybe-funcall (thing)
  (if (or (functionp thing)
          (and (symbolp thing)
               (fboundp thing)))
      (funcall thing)
      thing))

(defun smt/get-style (theme style)
  (smt/+ (smt/maybe-funcall
          (smt/t-base-style theme))
         (smt/maybe-funcall
          (funcall
           (intern (concat "smt/t-"
                           (substring (symbol-name style) 1)))
           theme))))

(defun smt/modeline-format ()
  (let ((theme (smt/get-current-theme)))
    (cond ( (smt/theme-p theme)
            (smt/t-export theme))
          ( (or (functionp theme)
                (symbolp theme))
            (funcall theme))
          ( (stringp theme)
            theme)
          ( t (error "Current theme has wrong type" theme)))))

(defun smt/get-current-theme ()
  (cdr (assoc smt/current-theme smt/themes)))

(defmacro smt/deftheme (name &rest pairs)
  `(let (( theme (make-smt/theme ,@pairs)))
     (setq smt/themes (cl-delete ',name smt/themes :key 'car)
           smt/themes (acons ',name theme smt/themes)
           smt/current-theme ',name)))
(put 'smt/deftheme 'common-lisp-indent-function
     '(1 &body))

(defun smt/reset ()
  (interactive)
  (ignore-errors
    (unload-feature 'svg-mode-line-themes t))
  (ignore-errors
    (unload-feature 'svg-mode-line-themes-styles t))
  (ignore-errors
    (unload-feature 'svg-mode-line-themes-core t))
  (require (quote svg-mode-line-themes)))

(provide 'svg-mode-line-themes-core)
;; svg-mode-line-themes-core.el ends here
