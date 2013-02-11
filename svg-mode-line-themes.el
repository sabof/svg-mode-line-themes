(require 'svg-mode-line-themes-core)
(require 'svg-mode-line-themes-widgets)
(require 'svg-mode-line-themes-diesel)
(require 'svg-mode-line-themes-nasa)
(require 'svg-mode-line-themes-black-crystal)
(defvar smt/line-format 'mode-line-format)

(setq smt/themes
      (acons 'default (default-value 'mode-line-format)
             smt/themes))

(setq smt/current-theme 'diesel)

(defun* smt/next-theme ()
  (interactive)
  (assert (> (length smt/themes) 1))
  (let* (( position (position smt/current-theme smt/themes :key 'car))
         ( next-theme
           (or (car (nth (1+ position) smt/themes))
               (car (nth 0 smt/themes)))))
    (setq smt/current-theme next-theme)
    (when (eq 'archetype smt/current-theme)
      (smt/next-theme)
      (return-from smt/next-theme))
    ;; (funcall (smt/theme-setup-hook (smt/get-current-theme)))
    (force-mode-line-update)
    (message "Current mode-line theme: %s" next-theme)))

(defun smt/set-theme (theme)
  (interactive
   (list (intern (completing-read
                  "Set mode-line theme to: "
                  (mapcar 'symbol-name (mapcar 'car smt/themes)) nil t))))
  (setq smt/current-theme theme)
  ;; (funcall (smt/theme-setup-hook (smt/get-current-theme)))
  (force-mode-line-update))

(defun smt/enable (&optional use-header-line)
  (set-default (if use-header-line
                   'header-line-format
                   'mode-line-format)
               '(:eval (smt/modeline-format)))
  ;; (funcall (smt/theme-setup-hook (smt/get-current-theme)))
  (force-mode-line-update))

(provide 'svg-mode-line-themes)
;; svg-mode-line-themes.el ends here
