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

(defun smt/default-buffer-indicators-text ()
  (let ((indicators
         (concat
          (unless (or (eq system-type 'windows-nt) (daemonp))
            "S")
          (when (window-dedicated-p) "D")
          (when buffer-read-only "R")
          (when (mfile-remote-p) " REMOTE")
          " ")))
    (if (< 1 (length indicators))
        indicators
        "")))

(defun smt/minor-mode-indicators ()
  (concat
   (when (bound-and-true-p es-aai-mode) "I")
   (when (or (bound-and-true-p evil-local-mode)
             (bound-and-true-p evil-mode)) "E")
   (when truncate-lines "T")
   (when dired-omit-mode "O")
   (when (bound-and-true-p save-auto-hook) "A")
   (when (bound-and-true-p wmi) "M")))

(smt/defwidget buffer-name
  :on-click (lambda (e)
              (interactive "e")
              (message (or (buffer-file-name)
                           (ignore-errors
                             (dired-current-directory)))))
  :text 'smt/default-buffer-name-text)

(smt/defwidget which-function
  :text (lambda ()
          (when (bound-and-true-p which-func-mode)
            (let ((text (format-mode-line which-func-current)))
              (when (and (> (length text) 0)
                         (not (equal text "???")))
                (concat " :: " text))))))

(smt/defwidget position-info
  :text (lambda ()
          (format-mode-line "%l:%p"))
  :on-click (lambda (e)
              (interactive "e")
              (message "Column: %s" (current-column))))

(smt/defrow default-left
  :widgets '(buffer-name))

(smt/defrow default-right
  :widgets '(position-info)
  :align 'right)

(provide 'svg-mode-line-themes-widgets)
;; svg-mode-line-themes-widgets.el ends here
