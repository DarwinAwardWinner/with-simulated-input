;;; -*- lexical-binding: t -*-

(require 'with-simulated-input)
(require 'ert)
(require 'cl-lib)

(ert-deftest simulate-input ()
  "Tests for the basic functionality of the `with-simulated-input' macro."
  ;; Basic string input
  (should
   (string= "hello"
            (with-simulated-input "hello RET"
              (read-string "Enter a string: "))))
  ;; Error if RET is not pressed to finish the input
  (should-error
   (with-simulated-input "hello"
     (read-string "Enter a string: ")))
  ;; Can throw an error manually
  (should-error
   (with-simulated-input "(error SPC \"Manually SPC throwing SPC an SPC error\") RET"
     (command-execute 'eval-expression)))
  ;; Extra keys should not cause errors
  (should
   (string= "hello"
            (with-simulated-input "hello RET M-x eval-expression (error SPC \"Manually SPC throwing SPC an SPC error\") RET"
              (read-string "Enter a string: ")))))

(provide 'with-simulated-input-test)

;;; ido-ubiquitous-test.el ends here
