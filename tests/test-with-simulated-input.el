;;; -*- lexical-binding: t -*-

(require 'with-simulated-input)
(require 'cl-lib)
(require 'buttercup)

;; Needs to be dynamically bound
(defvar mycollection)

(describe "`with-simulated-input'"
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
  (it "should allow aborting via C-g in KEYS"
    (expect
     (condition-case nil
         (with-simulated-input "C-g"
           (read-string "Enter a string: "))
       (quit 'caught-quit))
     :to-be 'caught-quit))


  (describe "used with `completing-read'"
    :var (collection completing-read-function)
    (before-each
      (setq mycollection '("bluebird" "blueberry" "bluebell" "bluegrass" "baseball")
            completing-read-function #'completing-read-default))
    ;; Unambiguous completion
    (it "should work with unambiguous tab completion"
      (expect
       ;; First TAB completes "blue", 2nd completes "bird"
       (with-simulated-input "bl TAB bi TAB RET"
         (completing-read "Choose: " mycollection))
       :to-equal "bluebird"))
    (it "should work with ambiguous tab completion"
      (expect
       (with-simulated-input "bl TAB C-j"
         (completing-read "Choose: " mycollection))
       :to-equal "blue"))
    (it "should fail to exit with ambiguous completion and `require-match'"
      (expect
       (lambda ()
         (with-simulated-input "bl TAB C-j"
           (completing-read "Choose: " mycollection nil t)))
       :to-throw)))

  (describe "using lisp forms in KEYS argument of `with-simulated-input'"
    (it "should allow evaluating arbitrary lisp forms"
      (expect
       (with-simulated-input '("hello SPC" (insert "world") "RET")
         (read-string "Enter a string: "))
       :to-equal "hello world"))
    (it "should allow lisp forms to throw errors"
      (expect
       (lambda ()
         (with-simulated-input '("hello SPC" (error "Throwing an error") "RET")
           (read-string "Enter a string: ")))
       :to-throw))
    (it "should not interpret lisp forms once BODY has finished"
      (expect
       (with-simulated-input '("hello SPC world RET RET"
                               (error "Should not reach this error"))
         (read-string "Enter a string: "))
       :to-equal "hello world"))))

;;; test-with-simulated-input.el ends here
