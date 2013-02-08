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

(setq smt/widgets
      (acons 'buffer-name
             (make-smt/widget
              :on-click (lambda (e)
                          (interactive "e"))
              :text 'smt/default-buffer-name-text)
             smt/widgets))

(setq smt/widgets
      (acons 'position-info
             (make-smt/widget
              :on-click (lambda (e)
                          (interactive "e"))
              :text (lambda ()
                      (format-mode-line "%l:%p")))
             smt/widgets))

(setq smt/rows
      (acons 'default-left
             (make-smt/row
              :widgets (list 'buffer-name))
             smt/rows))

(setq smt/rows
      (acons 'default-right
             (make-smt/row
              :widgets (list 'position-info)
              :alignment 'right)
             smt/rows))

(provide 'svg-mode-line-themes-widgets)
;; svg-mode-line-themes-widgets.el ends here
