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

(provide 'svg-mode-line-themes-tests)
;; svg-mode-line-themes-tests.el ends here
