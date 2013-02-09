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
              (interactive "e"))
  :text 'smt/default-buffer-name-text)

(smt/defwidget position-info
  :on-click (lambda (e)
              (interactive "e"))
  :text (lambda ()
          (format-mode-line "%l:%p")))

(smt/defrow default-left
  :widgets '(buffer-name))

(smt/defrow default-right
  :widgets '(position-info)
  :alignment 'right)

(provide 'svg-mode-line-themes-widgets)
;; svg-mode-line-themes-widgets.el ends here
