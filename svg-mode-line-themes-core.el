(require 'cl)
(or (require 'xmlgen nil t)
    (require 'xmlgen "xml-gen"))

;; (defvar smt/widgets nil)
;; (defvar smt/rows nil)
(defvar smt/themes nil)
(defvar smt/current-theme nil)

;;; Structs
(defmacro smt/deftree (name &rest props)
  (let (( maker-name
          (intern (concat "smt/make-"
                          (symbol-name name))))
        ( \definer-name
          (intern (concat "smt/def" (symbol-name name))))
        ( namespace-name
          (intern (concat "smt/" (symbol-name name) "s"))))
    `(progn
       (defvar ,namespace-name nil)
       (defun ,maker-name (&rest pairs)
         (unless (memq :parent pairs)
           (setf (getf pairs :parent) 'archetype))
         pairs)
       (defmacro ,definer-name (name &rest pairs)
         `(let* (( object (,',maker-name ,@pairs)))
            (setq ,',namespace-name (cl-delete ',name ,',namespace-name :key 'car)
                  ,',namespace-name (acons ',name object ,',namespace-name))))
       (put (quote ,definer-name) 'common-lisp-indent-function
            '(1 &body))
       (,definer-name archetype ,@props))))
(put 'smt/deftree 'common-lisp-indent-function
     '(1 &body))

(defun smt/get (object property &optional namespace)
  (when (and object (symbolp object))
    (setq object (cdr (assoc object namespace))))
  (cond ( (memq property object)
          (getf object property))
        ( (getf object :parent)
          (smt/get (getf object :parent)
                   property namespace))))

(defun smt/maybe-funcall (thing &rest args)
  (if (or (functionp thing)
          (and (symbolp thing)
               (fboundp thing)))
      (apply thing args)
      thing))

(smt/deftree theme
  :background nil
  :overlay nil
  :defs nil
  :export-func 'smt/t-export-default
  :base-style 'smt/default-base-style
  :local-widgets nil
  :rows nil)

(defun smt/t-background (theme)
  (smt/maybe-funcall
   (smt/get theme :background smt/themes)))

(defun smt/t-overlay (theme)
  (smt/maybe-funcall
   (smt/get theme :overlay smt/themes)))

(defun smt/t-defs (theme)
  (smt/maybe-funcall
   (smt/get theme :defs smt/themes)))

(defun smt/t-export (theme)
  (smt/maybe-funcall
   (smt/get theme :export-func smt/themes)
   theme))

(defun smt/t-base-style (theme)
  (smt/maybe-funcall
   (smt/get theme :base-style smt/themes)))

(defun smt/t-local-widgets (theme)
  (smt/maybe-funcall
   (smt/get theme :local-widgets smt/themes)))

(defun smt/t-rows (theme)
  (smt/maybe-funcall
   (smt/get theme :rows smt/themes)))

;;; Row

(smt/deftree row
  :align 'left
  :width-func 'smt/r-width-default
  :margin 0
  :widgets nil
  :base-style nil
  :export-func 'smt/r-export-default)

(defun smt/r-align (row)
  (smt/maybe-funcall
   (smt/get row :align smt/rows)))

(defun smt/r-width (row)
  (smt/maybe-funcall
   (smt/get row :width-func smt/rows)
   row))

(defun smt/r-margin (row)
  (smt/maybe-funcall
   (smt/get row :margin smt/rows)
   row))

(defun smt/r-widgets (row)
  (smt/maybe-funcall
   (smt/get row :widgets smt/rows)))

(defun smt/r-base-style (row)
  (smt/maybe-funcall
   (smt/get row :base-style smt/rows)))

(defun smt/r-export (row theme)
  (smt/maybe-funcall
   (smt/get row :export-func smt/rows)
   row theme))

;;; Widget

(smt/deftree widget
  :parent nil
  :style 'smt/default-base-style
  :on-click nil
  :text ""
  :width-func 'smt/w-width-default
  :export-func 'smt/w-export-default)

(defun smt/w-style (widget)
  (smt/maybe-funcall
   (smt/get widget :style smt/widgets)))

(defun smt/w-text (widget)
  (smt/maybe-funcall
   (smt/get widget :text smt/widgets)))

(defun smt/w-width (widget)
  (smt/maybe-funcall
   (smt/get widget :width-func smt/widgets)
   widget))

(defun smt/w-export (widget row theme)
  (smt/maybe-funcall
   (smt/get widget :export-func smt/widgets)
   widget row theme))

(defun smt/w-on-click (widget)
  (smt/get widget :on-click smt/widgets))

;;; Structs EOF
;;; Methods

(defun smt/ranges-overlap (r1 r2)
  (cond ( (<= (cdr r1) (car r2))
          nil)
        ( (<= (cdr r2) (car r1))
          nil)
        ( t t)))

(defun smt/r-range (row)
  (let (( left (smt/r-left row)))
    (cons left (+ left (smt/r-width row)))))

(defun smt/t-visible-rows (theme)
  (let* (( rows (mapcar (apply-partially 'smt/t-normalize-row theme)
                        (smt/t-rows theme))))
    (dotimes (iter (length rows))
      (when (nth iter rows)
        (let* (( current-row (nth iter rows))
               ( following-rows (last rows (- (length rows) 1 iter)))
               ( current-row-range
                 (smt/r-range current-row)))
          (dotimes (iter2 (length following-rows))
            (let (( following-row-range
                    (smt/r-range (nth iter2 following-rows))))
              (when (or (and (nth iter2 following-rows)
                             (smt/ranges-overlap
                              current-row-range
                              following-row-range))
                        (minusp (car following-row-range)))
                (setf (nth iter2 following-rows) nil)))))))
    (remove-if 'null rows)))

(defun smt/t-export-default-xml (theme)
  (let* (( width (smt/window-pixel-width))
         ( height (frame-char-height))
         ( rows (smt/t-visible-rows theme)))
    (xmlgen
     `(svg
       :xmlns "http://www.w3.org/2000/svg"
       :width ,width
       :height ,height
       ,@(smt/t-defs theme)
       ,@(smt/t-background theme)
       ,@(mapcar
          (lambda (row) (smt/r-export row theme))
          rows)
       ,@(smt/t-overlay theme)
       ))))

(defun* smt/t-export-default (theme)
  ;; (return-from smt/t-export-default)
  (let* ((xml (smt/t-export-default-xml theme))
         (image (create-image xml 'svg t)))
    (propertize
     "."
     'display image
     'keymap (let (( map (make-sparse-keymap)))
               (es-define-keys map
                 (kbd "<mouse-1>") 'smt/receive-click
                 (kbd "<nil> <header-line> <mouse-1>") 'smt/receive-click
                 (kbd "<nil> <mode-line> <mouse-1>") 'smt/receive-click
                 (kbd "<header-line> <mouse-1>") 'smt/receive-click
                 (kbd "<mode-line> <mouse-1>") 'smt/receive-click)
               map))))

(defun smt/r-width-default (row)
  (let (( widgets (smt/r-widgets row))
        ( total-width 0))
    (dolist (widget widgets)
      (setq widget (smt/t-normalize-widget
                    (smt/get-current-theme) widget))
      (incf total-width (smt/w-width widget)))
    total-width))

(defun smt/t-normalize-widget (theme widget-or-name)
  (if (listp widget-or-name)
      widget-or-name
      (or (cdr (assoc widget-or-name (smt/t-local-widgets theme)))
          (cdr (assoc widget-or-name smt/widgets))
          (error "Can't process widget: %s" widget-or-name))))

(defun smt/t-normalize-row (theme row-or-name)
  (if (listp row-or-name)
      row-or-name
      (or (cdr (assoc row-or-name smt/rows))
          (error "Can't process row: %s" row-or-name))))

(defun smt/r-export-default (row theme)
  `(text
    :text-anchor ,(progn
                   (case (smt/r-align row)
                     ( left "start")
                     ( right "end")))
    :x ,(progn
         (case ( smt/r-align row)
           ( left (* (smt/r-margin row)
                     (frame-char-width)))
           ( right (- (smt/window-pixel-width)
                      (* (smt/r-margin row)
                         (frame-char-width))))))
    :y ,(smt/text-base-line)
    ,@(mapcar (lambda (widget-or-name)
                (smt/w-export
                 (smt/t-normalize-widget
                  theme widget-or-name)
                 row theme))
              (smt/r-widgets row))))

(defun smt/w-export-default (widget row theme)
  `(tspan
    ,@(smt/+ (smt/t-base-style theme)
             (smt/r-base-style row)
             (smt/w-style widget))
    ,(smt/w-text widget)))

(defun smt/w-width-default (widget)
  (length (smt/w-text widget)))

(defun* smt/r-receive-click (row theme event)
  (setq row (smt/t-normalize-row theme row))
  (let* (( click-char-location (cadr (mouse-position)))
         ( window-width (smt/window-width))
         ( widgets (smt/r-widgets row))
         ( offset (smt/r-left row))
         current-widget-width)
    (dolist (widget widgets)
      (setq widget (smt/t-normalize-widget theme widget))
      (setq current-widget-width (smt/w-width widget))
      (when (and (<= offset click-char-location)
                 (< click-char-location
                    (+ offset current-widget-width)))
        (when (smt/w-on-click widget)
          (funcall (smt/w-on-click widget) event))
        (return-from smt/r-receive-click t))
      (setq offset (+ offset current-widget-width)))
    nil))

(defun* smt/t-receive-click (theme event)
  (let (( rows (smt/t-rows theme)))
    (dolist (row rows)
      (setq row (smt/t-normalize-row theme row))
      (when (smt/r-receive-click row theme event)
        (return-from smt/t-receive-click t)))
    (message "")))

(defun smt/receive-click (event)
  (interactive "e")
  (smt/t-receive-click
   (smt/get-current-theme)
   event))

(defun smt/r-left (row)
  (let (( margin (smt/r-margin row))
        ( width (smt/r-width row)))
    (if (eq 'left (smt/r-align row))
        margin
        (- (smt/window-width) (+ margin width)))))

;;; Methods EOF

(defun smt/window-pixel-width ()
  (let (( window-edges (window-pixel-edges)))
    (- (nth 2 window-edges) (nth 0 window-edges))))

(defun smt/window-width ()
  (let (( window-edges (window-edges)))
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
  (ceiling
   (smt/points-to-pixels
    (/ (face-attribute 'default :height) 10))))

(defun smt/text-base-line ()
  ;; Should be this one, but empirically it doesn't work as well
  ;; (smt/font-pixel-size)
  (let ((font-size (* 0.7 (smt/font-pixel-size))))
    (floor
     (+ font-size
        (/ (- (frame-char-height)
              font-size)
           2)))))

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

(defun smt/modeline-format ()
  (let ((theme (smt/get-current-theme)))
    (cond ( (or (functionp theme)
                (symbolp theme))
            (funcall theme))
          ( (listp theme)
            (smt/t-export theme))
          ( t theme))))

(defun smt/get-current-theme ()
  (cdr (assoc smt/current-theme smt/themes)))

(defun smt/get-widget-by-name (name)
  (cdr (assoc name smt/widgets)))

(defun smt/reset ()
  (interactive)
  (let (( tests-where-loaded
          (featurep 'svg-mode-line-themes-tests)))
    (ignore-errors
      (unload-feature 'svg-mode-line-themes t))
    (ignore-errors
      (unload-feature 'svg-mode-line-themes-styles t))
    (ignore-errors
      (unload-feature 'svg-mode-line-themes-widgets t))
    (ignore-errors
      (unload-feature 'svg-mode-line-themes-core t))
    (require (quote svg-mode-line-themes))
    (when tests-where-loaded
      (ignore-errors
        (unload-feature 'svg-mode-line-themes-tests t))
      (require (quote svg-mode-line-themes-tests)))))

(provide 'svg-mode-line-themes-core)
;; svg-mode-line-themes-core.el ends here
