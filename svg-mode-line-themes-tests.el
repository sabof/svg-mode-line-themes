(require 'svg-mode-line-themes)
(require 'ert)

(smt/defwidget test-center-label
  :text "center")

(smt/defrow center-test
  :align 'center
  :widgets '(test-center-label))

(smt/deftheme test
  :parent 'diesel
  :rows '(default-left
          center-test
          default-right
          ))

(defun smt-test/run-in-emacs-Q ()
  (interactive)
  (let* (( src `(progn
                  (setq debug-on-error t)
                  (defvar *memacs-folder* "/home/k/.emacs.d")
                  (defun mmake-path (arg &optional add-slash)
                    (save-match-data
                      (when (string-match "^/" arg)
                        (setq arg (substring arg 1))))
                    (concat (directory-file-name (concat *memacs-folder* "/" arg))
                            (if add-slash "/" "")))
                  (require 'package)
                  (setq package-user-dir (mmake-path "elpa"))
                  (package-initialize)
                  (setq package-archives
                        '(("ELPA" . "http://tromey.com/elpa/")
                          ("gnu" . "http://elpa.gnu.org/packages/")
                          ("melpa" . "http://melpa.milkbox.net/packages/")
                          ("marmalade" . "http://marmalade-repo.org/packages/")))
                  (let ((default-directory (mmake-path "site-lisp")))
                    (normal-top-level-add-subdirs-to-load-path)
                    (normal-top-level-add-to-load-path
                     (list (mmake-path "site-lisp") (mmake-path "")
                           "ess/lisp" "emacspeak/lisp"
                           "org-mode/lisp" "org-mode/contrib/lisp")))
                  (let ((default-directory (mmake-path "site-lisp/my-scripts")))
                    (normal-top-level-add-subdirs-to-load-path))
                  (require 'svg-mode-line-themes)
                  (smt/enable)
                  (setq default-directory (mmake-path "site-lisp/my-scripts"))
                  ))
         ( string (shell-quote-argument
                   (with-output-to-string
                       (print src)))))
    (async-shell-command (concat "emacs -Q --exec " string))))


(ert-deftest smt/+ ()
  (should (null (smt/+)))
  (should (null (smt/+ '(:a 45) '(:a nil))))
  (should (equal (smt/+ '(:a 45) nil)
                 '(:a 45)))
  (should (equal '(:a 45)
                 (smt/+ '(:a 45))))
  (let (( result (smt/+ '(:a 1) '(:b 2))))
    (should (= 1 (getf result :a)))
    (should (= 2 (getf result :b)))
    (should (= 4 (length result))))
  (let (( result (smt/+ '(:a 1) '(:a 2))))
    (should (= 2 (getf result :a)))
    (should (= 2 (length result))))
  (let (( result (smt/+ '(:a 1) '(:b 2) '(:c 3))))
    (should (= 1 (getf result :a)))
    (should (= 2 (getf result :b)))
    (should (= 3 (getf result :c)))
    (should (= 6 (length result)))))

(ert-deftest smt/ranges-overlap ()
  (should (smt/ranges-overlap '(0 . 10) '(5 . 6)))
  (should (smt/ranges-overlap '(0 . 10) '(9 . 20)))
  (should (smt/ranges-overlap '(3 . 10) '(3 . 5)))
  (should (smt/ranges-overlap '(3 . 5) '(3 . 10)))
  (should-not (smt/ranges-overlap '(0 . 0) '(0 . 0)))
  (should-not (smt/ranges-overlap '(0 . 10) '(10 . 10)))
  (should-not (smt/ranges-overlap '(0 . 0) '(0 . 10)))
  (should-not (smt/ranges-overlap '(0 . 10) '(0 . 0)))
  )

(ert-deftest smt/row ()
  (let (( row (smt/make-row
               :margin 2
               :widgets (list
                         (smt/make-widget
                          :text "123")
                         (smt/make-widget
                          :text "456")))))
    (should (= (smt/r-width row) 6))
    (should (eq (smt/r-align row) 'left))
    (should (= (smt/maybe-funcall (smt/r-margin row)) 2))
    (should (= (smt/r-left row) 2))
    (should (equal (smt/r-range row) '(2 . 8)))))

(ert-deftest smt/theme-overlapping ()
  (let (( theme
          (smt/make-theme
           :rows
           (list (smt/make-row
                  :margin 2
                  :widgets
                  (list
                   (smt/make-widget
                    :text "123346")))
                 (smt/make-row
                  :margin 8
                  :widgets
                  (list
                   (smt/make-widget
                    :text "123346")))))))
    (should (= 2 (length (smt/t-visible-rows theme)))))
  (let (( theme
          (smt/make-theme
           :rows
           (list (smt/make-row
                  :margin 2
                  :widgets
                  (list
                   (smt/make-widget
                    :text "123346")))
                 (smt/make-row
                  :margin 7
                  :widgets
                  (list
                   (smt/make-widget
                    :text "123346")))))))
    (should (= 1 (length (smt/t-visible-rows theme)))))
  (let (( theme
          (smt/make-theme
           :rows
           (list (smt/make-row
                  :widgets
                  (list
                   (smt/make-widget
                    :text "first")))
                 (smt/make-row
                  :align 'right
                  :widgets
                  (list
                   (smt/make-widget
                    :text "123456")))
                 (smt/make-row
                  :align 'right
                  :widgets
                  (list
                   (smt/make-widget
                    :text "1234567")))))))
    (flet (( smt/window-width () 10))
      (should (= 4 (smt/r-left (second (smt/t-rows theme)))))
      (should (= 1 (length (smt/t-visible-rows theme))))
      (should (equal (smt/w-text
                      (car (smt/r-widgets
                            (car (smt/t-visible-rows theme)))))
                     "first"))
      ))
  (let (( theme
          (smt/make-theme
           :rows
           (list (smt/make-row
                  :widgets
                  (list
                   (smt/make-widget
                    :text "12345")))
                 (smt/make-row
                  :align 'right
                  :widgets
                  (list
                   (smt/make-widget
                    :text "12345")))))))
    (flet (( smt/window-width () 10))
      (should (= 2 (length (smt/t-visible-rows theme)))))))

(ert-deftest smt/objects ()
  (let ((namespace (list (cons 'archetype (list :type 'type1 :shadow 'shadow))))
        (obj1 (list :parent 'archetype :prop1 1 :prop2 2 :prop3 3 :shadow nil)))
    (should (eq 'type1 (smt/get obj1 :type namespace)))
    (should (null (smt/get obj1 :shadow namespace)))))

(provide 'svg-mode-line-themes-tests)
;; svg-mode-line-themes-tests.el ends here
