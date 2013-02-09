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
  (align 'left)
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
;;; Methods

(defun smt/t-export (theme)
  (funcall (smt/t-export-func theme) theme))

(defun smt/r-export (row theme)
  (funcall (smt/r-export-func row) row theme))

(defun smt/w-export (widget row theme)
  (funcall (smt/w-export-func widget) widget row theme))

(defun smt/t-export-default-xml (theme)
  (let* (( width (smt/window-width))
         ( height (frame-char-height))
         ( rows (smt/t-rows theme)))
    (xmlgen
     `(svg
       :xmlns "http://www.w3.org/2000/svg"
       :width ,width
       :height ,height
       ,@(smt/maybe-funcall (smt/t-defs theme))
       ,@(smt/maybe-funcall (smt/t-background theme))
       ,@(mapcar
          (lambda (row-or-name)
            (smt/r-export
             (smt/t-normalize-row
              theme row-or-name)
             theme))
          (smt/t-rows theme))
       ,@(smt/maybe-funcall (smt/t-overlay theme))
       ))))

(defun smt/t-export-default (theme)
  (let* ((xml (smt/t-export-default-xml theme))
         (image (create-image xml 'svg t)))
    (propertize
     "."
     'display image
     'help-echo
     (or (buffer-file-name)
         (ignore-errors
           (dired-current-directory))))))

(defun smt/t-normalize-widget (theme widget-or-name)
  (if (smt/widget-p widget-or-name)
      widget-or-name
      (or (cdr (assoc widget-or-name (smt/t-local-widgets theme)))
          (cdr (assoc widget-or-name smt/widgets))
          (error "Can't process widget: %s" widget-or-name))))

(defun smt/t-normalize-row (theme row-or-name)
  (if (smt/row-p row-or-name)
      row-or-name
      (or (cdr (assoc row-or-name smt/rows))
          (error "Can't process row: %s" row-or-name))))

(defun smt/r-export-default (row theme)
  `(text
    :text-anchor ,(case
                   ( smt/r-align row)
                   ( left "start")
                   ( right "end")
                   ( center "middle"))
    :x ,(case
         ( smt/r-align row)
         ( left (* (smt/r-margin row)
                   (frame-char-width)))
         ( right (- (frame-pixel-width)
                    (* (smt/r-margin row)
                       (frame-char-width))))
         ( center (/ (frame-pixel-width) 2)))
    :y ,(smt/text-base-line)
    ,@(mapcar (lambda (widget-or-name)
                (smt/w-export
                 (smt/t-normalize-widget
                  theme widget-or-name)
                 row theme))
              (smt/r-widgets row))))

(defun smt/w-export-default (widget row theme)
  `(tspan
    ,@(smt/+ (smt/maybe-funcall (smt/t-base-style theme))
             (smt/maybe-funcall (smt/r-base-style row))
             (smt/maybe-funcall (smt/w-style widget)))
    ,(smt/maybe-funcall (smt/w-text widget))))

(defun smt/w-width-default (widget)
  (length (smt/maybe-funcall (smt/w-text widget))))

(defun smt/w-receive-click (widget row theme event)
  )

(defun smt/r-receive-click (row theme event)
  (let* (( widgets (smt/r-widgets row))
         ( align (smt/r-align row))
         ( offset (smt/maybe-funcall (smt/r-margin row))))

    ))

(defun smt/t-receive-click (theme event)
  (let (( rows (smt/t-rows theme)))
    (mapcar )
    (dolist (row ))))

(defmacro smt/define-struct-copy-modifier (accessor-prefix)
  `(defmacro ,(intern (concat accessor-prefix "copy-and-modify"))
       (struct &rest properties)
     (let (( new-struct (gensym "new-struct-"))
           result)
       `(let (( ,new-struct (smt/copy-struct ,struct)))
          ,@(progn
             (while properties
               (push `(setf
                       (,(intern
                          (concat
                           ,accessor-prefix
                           (substring
                            (symbol-name
                             (pop properties))
                            1)))
                         ,new-struct)
                       (pop properties))
                     result))
             (nreverse result))
          ,new-struct))))

(smt/define-struct-copy-modifier "smt/t-")
(smt/define-struct-copy-modifier "smt/w-")
(smt/define-struct-copy-modifier "smt/r-")

;;; Methods EOF

(defun smt/window-width ()
  (let (( window-edges (window-pixel-edges)))
    (- (nth 2 window-edges) (nth 0 window-edges))))

(defun smt/copy-struct (struct)
  (funcall
    (intern
     (concat
      "copy-"
      (substring
       (symbol-name
        (aref 0 struct))
       10)))
    struct))

(defun smt/points-to-pixels (points)
  ;; points = pixels * 72 / 96
  (/ (* 96 points) 72))

(defun smt/font-pixel-size ()
  (round
   (smt/points-to-pixels
    (/ (face-attribute 'default :height) 10))))

(defun smt/text-base-line ()
  ;; (smt/font-pixel-size)
  )

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
    ( t (let (( plistC (copy-list (car plists)))
              ( plistB (cadr plists))
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

(defun smt/modeline-format ()
  (let ((theme (smt/get-current-theme)))
    (cond ( (smt/theme-p theme)
            (smt/t-export theme))
          ( (or (functionp theme)
                (symbolp theme))
            (funcall theme))
          ( (stringp theme)
            theme)
          ( t (error "Can't process current theme: %s"
                     theme)))))

(defun smt/get-current-theme ()
  (cdr (assoc smt/current-theme smt/themes)))

(defun smt/get-widget-by-name (name)
  (cdr (assoc name smt/widgets)))

(defmacro smt/deftheme (name &rest pairs)
  `(let (( theme (make-smt/theme ,@pairs)))
     (setq smt/themes (cl-delete ',name smt/themes :key 'car)
           smt/themes (acons ',name theme smt/themes)
           smt/current-theme ',name)))
(put 'smt/deftheme 'common-lisp-indent-function
     '(1 &body))

(defmacro smt/defwidget (name &rest pairs)
  `(let (( widget (make-smt/widget ,@pairs)))
     (setq smt/widgets (cl-delete ',name smt/widgets :key 'car)
           smt/widgets (acons ',name widget smt/widgets)
           smt/current-widget ',name)))
(put 'smt/defwidget 'common-lisp-indent-function
     '(1 &body))

(defmacro smt/defrow (name &rest pairs)
  `(let (( row (make-smt/row ,@pairs)))
     (setq smt/rows (cl-delete ',name smt/rows :key 'car)
           smt/rows (acons ',name row smt/rows)
           smt/current-row ',name)))
(put 'smt/defrow 'common-lisp-indent-function
     '(1 &body))

(defun smt/reset ()
  (interactive)
  (let (( tests-where-loaded
          (featurep 'svg-mode-line-themes-tests)))
    (ignore-errors
      (unload-feature 'svg-mode-line-themes t))
    (ignore-errors
      (unload-feature 'svg-mode-line-themes-styles t))
    (ignore-errors
      (unload-feature 'svg-mode-line-themes-core t))
    (require (quote svg-mode-line-themes))
    (when tests-where-loaded
      (ignore-errors
        (unload-feature 'svg-mode-line-themes-tests t))
      (require (quote svg-mode-line-themes-tests)))))

(provide 'svg-mode-line-themes-core)
;; svg-mode-line-themes-core.el ends here
