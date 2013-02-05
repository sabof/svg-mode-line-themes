(defun es-svg-modeline-xml (text-left text-right)
  (let* ((horizontal-margin 10)
         (width (frame-pixel-width))
         (height (frame-char-height))
         (text-base-line (- height 4))
         (gradient "<defs>
<linearGradient id=\"grad1\" x1=\"0%\" y1=\"0%\" x2=\"0%\" y2=\"100%\">
 <stop offset=\"0%\" style=\"stop-color:rgb(255,255,255);stop-opacity:0.3\" />
         <stop offset=\"100%\" style=\"stop-color:rgb(0,0,0);stop-opacity:0.3\" />
         </linearGradient>
         </defs>")
         (dropshadow
          "<filter id=\"dropshadow\" height=\"130%\">
                     <feGaussianBlur in=\"SourceAlpha\" stdDeviation=\"3\"/> <!-- stdDeviation is how much to blur -->
                     <feOffset dx=\"2\" dy=\"2\" result=\"offsetblur\"/> <!-- how much to offset -->
                     <feMerge>
                     <feMergeNode/> <!-- this contains the offset blurred image -->
                     <feMergeNode in=\"SourceGraphic\"/> <!-- this contains the element that the filter is applied to -->
                     </feMerge>
                     </filter>")
         (text-props
          (format "y=\"%s\" font-weight=\"bold\" font-family=\"garamond\" style=\"filter:url(#dropshadow)\""
                  text-base-line))
         (left-aligned-text
          (format "<text x=\"%s\" %s text-anchor=\"start\">%s</text>"
                  horizontal-margin text-props text-left))
         (right-aligned-text
          (format "<text x=\"%s\" %s text-anchor=\"end\">%s</text>"
                  (- width horizontal-margin) text-props text-right))
         (background
          (format "<rect width=\"100%%\" height=\"100%%\" x=\"0\" y=\"0\" fill=\"url(#grad1)\"/>
    <rect width=\"100%%\" height=\"1\" x=\"0\" y=\"0\" fill=\"white\" fill-opacity=\"0.3\"/>
    <rect width=\"100%%\" height=\"1\" x=\"0\" y=\"%s\" fill=\"black\" fill-opacity=\"0.3\"/>"
                  (1- height))))
    (format "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%s\" height=\"%s\" version=\"1.1\">
    %s
    %s
    %s
    %s
    %s
    </svg>"
            width height dropshadow gradient background left-aligned-text right-aligned-text)))

(defun es-svg-modeline-format-xmlize (input)
  (let ((pairs '(("&" . "&amp;")
                 ("<" . "&lt;")
                 (">" . "&gt;"))))
    (dolist (pair pairs)
      (setq input (replace-in-string
                   input (car pair) (cdr pair))))
    input))

(defun es-svg-modeline-format ()
  (let* ((image (create-image (es-svg-modeline-xml
                               (es-svg-modeline-format-xmlize
                                (format-mode-line "%b"))
                               "Right") 'svg t)))
    (propertize "." 'display image)))

(defun es-svg-modeline-set ()
  (interactive)
  (setq mode-line-format '(:eval (es-svg-modeline-format))))


(provide 'es-svg-modeline)
;; es-svg-modeline.el ends here
