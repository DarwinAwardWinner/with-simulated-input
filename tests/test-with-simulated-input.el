;;; -*- lexical-binding: t -*-

(require 'with-simulated-input)
(require 'cl-lib)
(require 'buttercup)

(describe "Basic functionality of `with-simulated-input'"
  (it "should work for basic string input"
    (expect
     (with-simulated-input "hello RET"
       (read-string "Enter a string: "))
     :to-equal "hello"))
  (it "should throw an error if the input is incomplete"
    (expect
     (lambda ()
       (with-simulated-input "hello"
         (read-string "Enter a string: ")))
     :to-throw))
  (it "should allow the input to trigger errors"
    (expect
     (lambda ()
       (with-simulated-input
           "(error SPC \"Manually SPC throwing SPC an SPC error\") RET"
         (command-execute 'eval-expression)))
     :to-throw))
  (it "should ignore extra input after BODY has completed"
    (expect
     (with-simulated-input
         "hello RET M-x eval-expression (error SPC \"Manually SPC throwing SPC an SPC error\") RET"
       (read-string "Enter a string: "))
     :to-equal "hello"))
  (it "should allow multiple functions in BODY to read input"
    (expect
     (with-simulated-input "hello RET world RET"
       (list (read-string "First word: ")
             (read-string "Second word: ")))
     :to-equal '("hello" "world")))
  (it "should not allow `C-g' in the input"
    (expect
     (lambda ()
       (with-simulated-input "C-g" (ignore)))
     :to-throw)))

(describe "Using `with-simulated-input' with `completing-read'"
  :var ((collection '("bluebird" "blueberry" "bluebell" "bluegrass" "baseball"))
        (completing-read-function #'completing-read-default))
  ;; Unambiguous completion
  (it "should work with unambiguous tab completion"
    (expect
     ;; First TAB completes "blue", 2nd completes "bird"
     (with-simulated-input "bl TAB bi TAB RET"
       (completing-read "Choose: " collection))
     :to-equal "bluebird"))
  (it "should work with ambiguous tab completion"
    (expect
     (with-simulated-input "bl TAB C-j"
       (completing-read "Choose: " collection))
     :to-equal "blue"))
  (it "should fail to exit with ambiguous completion and `require-match'"
    (expect
     (lambda ()
       (with-simulated-input "bl TAB C-j"
         (completing-read "Choose: " collection nil t)))
     :to-throw)))

;;; test-with-simulated-input.el ends here
