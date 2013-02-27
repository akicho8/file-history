;; -*- compile-command: "emacs --script file-history-test.el" -*-

(setq load-path (cons ".." load-path))
(setq load-path (cons "." load-path))

(require 'ert)

(require 'file-history)

(ert-deftest test-file-history ()
  (should
   t
   ;; これどうやってテスト書けば？
   ;; (file-history)
   ))

(ert-run-tests-batch t)
