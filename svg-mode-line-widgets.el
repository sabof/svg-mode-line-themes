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

(setq smt/rows
      (acons 'default-left
             (make-smt/row
              :widgets (list 'buffer-name)
              )
             smt/rows))

(provide 'svg-mode-line-widgets)
;; svg-mode-line-widgets.el ends here
