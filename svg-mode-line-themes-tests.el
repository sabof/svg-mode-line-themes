(require 'ert)

(smt/deftheme widget-test
  :defs (smt/filter-inset 0.5 0.3)
  :background 'smt/bg-grey1
  :rows (list 'default-left 'default-right 'test-center)
  :overlay 'smt/bg-grey1-top
  ;; :local-widgets `((wig1 . ,(make-smt/widget :text "wig1")))
  )

(setq smt/rows
      (acons 'test-center
             (make-smt/row
              :widgets (list 'buffer-name)
              :alignment 'center)
             smt/rows))

(ert-deftest test-smt/+ ()
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

(provide 'svg-mode-line-themes-tests)
;; svg-mode-line-themes-tests.el ends here
