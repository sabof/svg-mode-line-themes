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
  (should (equal '(:a 45) (smt/+ '(:a 45))))
  (let (( result (smt/+ '(:a 1) '(:b 2))))
    (should (equal (getf result :a) 1))
    (should (equal (getf result :b) 2))
    (should (equal (length result) 4)))
  (let (( result (smt/+ '(:a 1) '(:a 2))))
    (should (equal (getf result :a) 2))
    (should (equal (length result) 2)))
  (let (( result (smt/+ '(:a 1) '(:b 2) '(:c 3))))
    (should (equal (getf result :a) 1))
    (should (equal (getf result :b) 2))
    (should (equal (getf result :c) 3))
    (should (equal (length result) 6)))
  )

(provide 'svg-mode-line-themes-tests)
;; svg-mode-line-themes-tests.el ends here
