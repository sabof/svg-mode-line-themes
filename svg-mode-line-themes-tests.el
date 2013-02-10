(require 'ert)

(smt/defrow test-center
  :widgets (list 'buffer-name
                 (make-smt/widget
                  :text " hello"))
  :align 'center
  :base-style (lambda ()
                (smt/+
                 (smt/default-base-style)
                 `(:filter
                   "url(#inset)"
                   :fill "#ff0000"))))

(smt/deftheme widget-test
  :defs (smt/filter-inset 0.5 0.3)
  :background 'smt/bg-grey1
  :base-style (lambda ()
                (smt/+
                 (smt/default-base-style)
                 `(:filter
                   "url(#inset)"
                   :fill "#404448")))
  :rows (list 'default-left 'default-right 'test-center)
  :overlay 'smt/bg-grey1-top
  ;; :local-widgets `((wig1 . ,(make-smt/widget :text "wig1")))
  )

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
  (should (null (smt/ranges-overlap '(0 . 10) '(10 . 10))))
  )

(ert-deftest smt/row ()
  (let (( row (make-smt/row
               :margin 2
               :widgets (list
                         (make-smt/widget
                          :text "123")
                         (make-smt/widget
                          :text "456")))))
    (should (= (smt/r-width row) 6))
    (should (eq (smt/r-align row) 'left))
    (should (= (smt/maybe-funcall (smt/r-margin row)) 2))
    (should (= (smt/r-left row) 2))
    (should (equal (smt/r-range row) '(2 . 8)))))

(ert-deftest smt/theme ()
  (let (( theme
          (make-smt/theme
           :rows
           (list (make-smt/row
                  :margin 2
                  :widgets
                  (list
                   (make-smt/widget
                    :text "123346")))
                 (make-smt/row
                  :margin 8
                  :widgets
                  (list
                   (make-smt/widget
                    :text "123346")))))))
    (should (= 2 (length (smt/t-non-overlapping-rows theme)))))
  (let (( theme
          (make-smt/theme
           :rows
           (list (make-smt/row
                  :margin 2
                  :widgets
                  (list
                   (make-smt/widget
                    :text "123346")))
                 (make-smt/row
                  :margin 7
                  :widgets
                  (list
                   (make-smt/widget
                    :text "123346")))))))
    (should (null (= 2 (length (smt/t-non-overlapping-rows theme)))))))

(provide 'svg-mode-line-themes-tests)
;; svg-mode-line-themes-tests.el ends here
