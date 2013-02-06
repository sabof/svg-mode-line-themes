(require 'cl)
(or (require 'xmlgen nil t)
    (require 'xmlgen "xml-gen"))

(defvar smt/themes nil)
(defvar smt/current-theme nil)
(defun es-mt/window-width ()
  (let (( window-edges (window-pixel-edges)))
    (- (nth 2 window-edges) (nth 0 window-edges))))

(defun smt/minor-mode-indicators ()
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

(defun es-mt/default-base-style ()
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

(defun smt/default-buffer-indicators-text ()
  (let ((indicators
         (concat
          (unless (or (eq system-type 'windows-nt) (daemonp))
            "S")
          (when (window-dedicated-p) "D")
          (when buffer-read-only "R")
          (when (mfile-remote-p) " REMOTE")
          " ")))
    (if (> (length indicators) 1)
        indicators
        "")))

(defun smt/default-buffer-name-text ()
  (let (( project-name
          (esprj-project-name
           (esprj-file-project
            (or (buffer-file-name)
                (ignore-errors (dired-current-directory)))))))
    (concat
     (when project-name (concat project-name " => "))
     (format-mode-line "%b")
     (if (and (or (buffer-file-name)
                  buffer-offer-save)
              (buffer-modified-p))
         "*"))))

(defun smt/get-style (theme style)
  (smt/+ (smt/maybe-funcall
          (smt/theme-base-style theme))
         (smt/maybe-funcall
          (funcall
           (intern (concat "smt/theme-"
                           (substring (symbol-name style) 1)))
           theme))))

(defun smt/left-text-width (theme)
  (+ (smt/theme-margin theme)
     (length
      (concat
       (smt/maybe-funcall
        (smt/theme-buffer-name-text theme))
       (smt/maybe-funcall
        (smt/theme-buffer-indicators-text theme))))))

(defun smt/right-text-width (theme)
  (+ (smt/theme-margin theme)
     (smt/theme-position-width theme)
     (length
      (concat
       (smt/maybe-funcall
        (smt/theme-major-mode-text theme))
       (smt/maybe-funcall
        (smt/theme-minor-mode-text theme))
       (smt/maybe-funcall
        (smt/theme-vc-text theme))))
     ;; Variable-width text safety-margin
     6))

(defun smt/default-xml-coverter (theme)
  (assert (smt/theme-p theme))
  (let* (( width (es-mt/window-width))
         ( height (frame-char-height))
         ( text-base-line
           (es-mt/text-base-line))
         ( horizontal-pixel-margin
           (* (smt/theme-margin theme)
              (frame-char-width)))
         ( left-width
           (funcall (smt/theme-left-text-width theme)
                    theme))
         ( right-width
           (funcall (smt/theme-right-text-width theme)
                    theme))
         ( position-width
           (smt/maybe-funcall
            (smt/theme-position-width theme)))
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
       ,@(smt/maybe-funcall (smt/theme-defs theme))
       ,@(smt/maybe-funcall (smt/theme-background theme))
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
                   ,(smt/maybe-funcall (smt/theme-major-mode-text theme)))
                  ;; Version Control
                  (tspan
                   ,@(smt/get-style theme :vc-style)
                   ,(smt/maybe-funcall (smt/theme-vc-text theme))
                   " ")
                  ;; Minor Modes
                  (tspan
                   ,@(smt/get-style theme :minor-mode-style)
                   ,(smt/maybe-funcall (smt/theme-minor-mode-text theme))))))
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
         ,(smt/maybe-funcall (smt/theme-buffer-indicators-text theme)))
        (tspan
         ,@(smt/get-style theme :buffer-name-style)
         ,(smt/maybe-funcall (smt/theme-buffer-name-text theme))))
       ,@(smt/maybe-funcall (smt/theme-overlay theme))))))

(defstruct smt/theme
  name
  background
  overlay
  defs
  (margin 2)
  (position-width 12)
  (xml-converter 'smt/default-xml-coverter)
  (setup-hook 'ignore)

  (base-style 'es-mt/default-base-style)
  buffer-name-style
  buffer-indicators-style
  vc-style
  position-style
  (left-text-width 'smt/left-text-width)
  (right-text-width 'smt/right-text-width)
  minor-mode-style
  major-mode-style

  (vc-text (lambda () (bound-and-true-p vc-mode)))
  (major-mode-text (lambda () (format-mode-line "%m")))
  (minor-mode-text 'smt/minor-mode-indicators)
  (buffer-name-text 'smt/default-buffer-name-text)
  (buffer-indicators-text 'smt/default-buffer-indicators-text))

(defun smt/modeline-format ()
  (let* ((theme (cdr (assoc smt/current-theme smt/themes)))
         ( image
           (create-image
            (funcall (smt/theme-xml-converter
                      theme)
                     theme)
            'svg t)))
    (propertize
     "."
     'display image
     'help-echo
     (or (buffer-file-name)
         (ignore-errors
           (dired-current-directory))))))

(defun smt/get-current-theme ()
  (cdr (assoc smt/current-theme smt/themes)))

(defmacro smt/deftheme (name &rest pairs)
  `(let ((theme (make-smt/theme)))
     (setf (smt/theme-name theme) ',name)
     ,@(progn
        (let (result)
          (while pairs
            (push `(setf (,(intern
                            (concat
                             "smt/theme-"
                             (substring
                              (symbol-name (pop pairs))
                              1)))
                           theme)
                         ,(pop pairs))
                  result))
          (nreverse result)))
     (setq smt/themes
           (cl-delete ',name smt/themes :key 'car))
     (setq smt/themes
           (acons ',name theme smt/themes))
     (setq smt/current-theme ',name)))
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
