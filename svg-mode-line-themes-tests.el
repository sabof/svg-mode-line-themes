(require 'cl)
(require 'svg-mode-line-themes)
(require 'ert)

;; (smt/defrow test-center
;;   :widgets (list 'buffer-name
;;                  (smt/make-widget
;;                   :text " hello"))
;;   :align 'center
;;   :style (lambda ()
;;                 (smt/+
;;                  (smt/default-base-style)
;;                  `(:filter
;;                    "url(#inset)"
;;                    :fill "#ff0000"))))

;; (smt/deftheme widget-test
;;   :defs (smt/filter-inset 0.5 0.3)
;;   :background 'smt/diesel-bg
;;   :style (lambda ()
;;                 (smt/+
;;                  (smt/default-base-style)
;;                  `(:filter
;;                    "url(#inset)"
;;                    :fill "#404448")))
;;   :rows (list 'default-left 'default-position 'default-right)
;;   :overlay 'smt/diesel-overlay
;;   ;; :local-widgets `((wig1 . ,(smt/make-widget :text "wig1")))
;;   )

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
  (let (( row (make-smt/row
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
          (make-smt/theme
           :rows
           (list (make-smt/row
                  :margin 2
                  :widgets
                  (list
                   (smt/make-widget
                    :text "123346")))
                 (make-smt/row
                  :margin 8
                  :widgets
                  (list
                   (smt/make-widget
                    :text "123346")))))))
    (should (= 2 (length (smt/t-visible-rows theme)))))
  (let (( theme
          (make-smt/theme
           :rows
           (list (make-smt/row
                  :margin 2
                  :widgets
                  (list
                   (smt/make-widget
                    :text "123346")))
                 (make-smt/row
                  :margin 7
                  :widgets
                  (list
                   (smt/make-widget
                    :text "123346")))))))
    (should (= 1 (length (smt/t-visible-rows theme)))))
  (let (( theme
          (make-smt/theme
           :rows
           (list (make-smt/row
                  :widgets
                  (list
                   (smt/make-widget
                    :text "first")))
                 (make-smt/row
                  :align 'right
                  :widgets
                  (list
                   (smt/make-widget
                    :text "123456")))
                 (make-smt/row
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
          (make-smt/theme
           :rows
           (list (make-smt/row
                  :widgets
                  (list
                   (smt/make-widget
                    :text "12345")))
                 (make-smt/row
                  :align 'right
                  :widgets
                  (list
                   (smt/make-widget
                    :text "12345")))))))
    (flet (( smt/window-width () 10))
      (should (= 2 (length (smt/t-visible-rows theme)))))))

(ert-deftest smt/objects ()
  )

(provide 'svg-mode-line-themes-tests)
;; svg-mode-line-themes-tests.el ends here
