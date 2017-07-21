;;; -*- lexical-binding: t -*-

(require 'with-simulated-input)
(require 'ert)
(require 'cl-lib)

(ert-deftest read-simulated-input ()
  :tags '(with-simulated-input)
  "Tests for the basic functionality of the `with-simulated-input' macro."
  ;; Basic string input
  (should
   (string=
    "hello"
    (with-simulated-input "hello RET"
      (read-string "Enter a string: "))))
  ;; Error if RET is not pressed to finish the input
  (should-error
   (with-simulated-input "hello"
     (read-string "Enter a string: ")))
  ;; Can throw an error manually
  (should-error
   (with-simulated-input
       "(error SPC \"Manually SPC throwing SPC an SPC error\") RET"
     (command-execute 'eval-expression)))
  ;; Extra keys should not cause errors
  (should
   (string=
    "hello"
    (with-simulated-input
        "hello RET M-x eval-expression (error SPC \"Manually SPC throwing SPC an SPC error\") RET"
              (read-string "Enter a string: "))))
  ;; A single input sequence can feed multiple functions
  (should
   (equal
    '("hello" "world")
    (with-simulated-input "hello RET world RET"
      (list (read-string "First word: ")
            (read-string "Second word: "))))))

(ert-deftest complete-with-simulated-input ()
  :tags '(with-simulated-input)
  "Tests for `with-simulated-input' with `completing-read'."
  (let ((collection '("bluebird" "blueberry" "bluebell" "bluegrass" "baseball"))
        (completing-read-function #'completing-read-default))
    ;; Unambiguous completion
    (should
     (string=
      "bluebird"
      ;; First TAB completes "blue", 2nd completes "bird"
      (with-simulated-input "bl TAB bi TAB RET"
        (completing-read "Choose: " collection))))
    ;; Ambiguous completion
    (should
     (string=
      "blue"
      (with-simulated-input "bl TAB C-j"
        (completing-read "Choose: " collection))))
    ;; Ambiguous completion with require-match
    (should-error
     (with-simulated-input "bl TAB C-j"
       (completing-read "Choose: " collection nil t)))))

(provide 'with-simulated-input-test)

;;; ido-ubiquitous-test.el ends here
