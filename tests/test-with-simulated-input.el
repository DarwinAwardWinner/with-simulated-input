;;; -*- lexical-binding: t -*-

(require 'undercover)
(undercover "with-simulated-input.el")

(require 'with-simulated-input)
(require 'cl-lib)
(require 'buttercup)

;; Needs to be dynamically bound
(defvar my-collection)
(defvar my-non-lexical-var)

(defun call-wsi-from-bytecomp-fun ()
  (with-simulated-input "hello SPC world RET"
    (read-string "Say hello: ")))
(byte-compile 'call-wsi-from-bytecomp-fun)

(describe "`wsi-get-unbound-key'"
  (it "should find an unbound key"
    (let ((unbound-key (wsi-get-unbound-key)))
      (expect unbound-key :to-be-truthy)
      (expect (wsi-key-bound-p unbound-key) :not :to-be-truthy)))
  (it "should report an error if it fails to find an unbound key"
    ;; Now we call it with an empty list of modifiers and keys to
    ;; search, so it definitely should not find a binding.
    (expect (wsi-get-unbound-key '() "")
            :to-throw 'error)))

(describe "`with-simulated-input'"

  (it "should work for basic string input"
    (expect
     (with-simulated-input "hello RET"
       (read-string "Enter a string: "))
     :to-equal "hello"))

  (it "should throw an error if the input is incomplete"
    (expect
     (with-simulated-input "hello"
       (read-string "Enter a string: "))
     :to-throw))

  (it "should allow the input to trigger errors"
    (expect

     (with-simulated-input
         "(error SPC \"Manually SPC throwing SPC an SPC error\") RET"
       (command-execute 'eval-expression))
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

  ;; https://github.com/DarwinAwardWinner/with-simulated-input/issues/4
  (it "should work inside code that switches buffer (issue #4)"
    (let ((orig-current-buffer (current-buffer)))
      (with-temp-buffer
        (let ((temp-buffer (current-buffer)))
          (with-simulated-input "a" (read-char))
          (expect (current-buffer) :to-equal temp-buffer)
          (expect (current-buffer) :not :to-equal orig-current-buffer)))))

  (it "should work in byte-compiled code (issue #6)"
    (expect (call-wsi-from-bytecomp-fun)
            :not :to-throw))

  (describe "used with `completing-read'"

    :var (completing-read-function)

    (before-each
      (setq my-collection '("bluebird" "blueberry" "bluebell" "bluegrass" "baseball")
            completing-read-function #'completing-read-default))

    ;; Unambiguous completion
    (it "should work with unambiguous tab completion"
      (expect
       ;; First TAB completes "blue", 2nd completes "bird"
       (with-simulated-input "bl TAB bi TAB RET"
         (completing-read "Choose: " my-collection))
       :to-equal "bluebird"))

    (it "should work with ambiguous tab completion"
      (expect
       (with-simulated-input "bl TAB C-j"
         (completing-read "Choose: " my-collection))
       :to-equal "blue"))

    (it "should fail to exit with ambiguous completion and `require-match'"
      ;; Suppress messages by replacing `message' with a stub
      (spy-on 'message)
      (expect

       (with-simulated-input "bl TAB C-j"
         (completing-read "Choose: " my-collection nil t))
       :to-throw)))

  (describe "using lisp forms in KEYS argument of `with-simulated-input'"

    (it "should allow evaluating arbitrary lisp forms"
      (expect
       (with-simulated-input '("hello SPC" (insert "world") "RET")
         (read-string "Enter a string: "))
       :to-equal "hello world"))

    (it "should allow KEYS to be evaluated at run time"
      (let ((greeting "hello")
            (target "world"))
        (expect
         (with-simulated-input
             (list greeting "SPC"
                   (list 'insert target)
                   "RET")
           (read-string "Say hello: "))
         :to-equal "hello world")))

    (it "should allow lisp forms to throw errors"
      (expect

       (with-simulated-input '("hello SPC" (error "Throwing an error") "RET")
         (read-string "Enter a string: "))
       :to-throw))

    (it "should not interpret lisp forms once BODY has finished"
      (expect
       (with-simulated-input '("hello SPC world RET RET"
                               (error "Should not reach this error"))
         (read-string "Enter a string: "))
       :to-equal "hello world"))

    (it "should evaluate lisp forms in the proper lexical environment"
      (let ((my-lexical-var nil))
        (with-simulated-input '("hello"
                                (setq my-lexical-var t)
                                "RET")
          (read-string "Enter a string: "))
        (expect my-lexical-var
                :to-be-truthy)))

    (it "should work in a non-lexical environment"
      (let ((my-non-lexical-var nil))
        (eval
         '(with-simulated-input '("hello"
                                  (setq my-non-lexical-var t)
                                  "RET")
            (read-string "Enter a string: "))
         nil)
        (expect my-non-lexical-var
                :to-be-truthy)))

    (it "should allow interpolation of variables into KEYS"
      (let ((my-key-sequence "hello")
            (my-lisp-form '(insert " world")))
        (expect
         (with-simulated-input (list
                                my-key-sequence
                                my-lisp-form
                                "RET")
           (read-string "Enter a string: "))
         :to-equal "hello world")))))

(defun time-equal-p (t1 t2)
  "Return non-nil if T1 and T2 represent the same time.

Note that there are multiple ways to represent a time, so
`time-equal-p' does not necessarily imply `equal'."
  (not (or (time-less-p t1 t2)
           (time-less-p t2 t1))))

(defvar canary-idle-time nil)
(defun idle-canary ()
  (setq canary-idle-time (current-idle-time)))
(defvar timers-to-cancel nil)
(defvar orig-timer--activate (symbol-function 'timer--activate))

(describe "`wsi-simulate-idle-time'"

  (before-each
    (setq canary-idle-time nil)
    (spy-on 'idle-canary :and-call-through)
    (spy-on 'timer--activate
            :and-call-fake
            (lambda (timer &rest args)
              (push timer timers-to-cancel)
              (apply orig-timer--activate timer args))))

  (after-each
    (mapc #'cancel-timer timers-to-cancel)
    (setq timers-to-cancel nil)
    (spy-calls-reset 'idle-canary))

  (it "should run idle timers"
    (run-with-idle-timer 500 nil 'idle-canary)
    (wsi-simulate-idle-time 500)
    (expect 'idle-canary :to-have-been-called))

  (it "should not run idle timers with longer times even when called multiple times"
    (run-with-idle-timer 500 nil 'set 'idle-canary)
    (wsi-simulate-idle-time 400)
    (wsi-simulate-idle-time 400)
    (wsi-simulate-idle-time 400)
    (expect 'idle-canary :not :to-have-been-called))

  (it "should run idle timers added by other idle timers"
    (run-with-idle-timer
     100 nil 'run-with-idle-timer
     200 nil 'idle-canary)
    (wsi-simulate-idle-time 500)
    (expect 'idle-canary :to-have-been-called))

  (it "should run idle timers added by other idle timers when the new timer is in the past"
    (run-with-idle-timer
     100 nil 'run-with-idle-timer
     90 nil 'run-with-idle-timer
     80 nil 'run-with-idle-timer
     70 nil 'run-with-idle-timer
     60 nil 'run-with-idle-timer
     50 nil 'idle-canary)
    (wsi-simulate-idle-time 110)
    (expect 'idle-canary :to-have-been-called))

  (it "should run all idle timers when called with SECS = nil"
    (run-with-idle-timer 1000 nil 'idle-canary)
    (wsi-simulate-idle-time 1)
    (expect 'idle-canary :not :to-have-been-called)
    (wsi-simulate-idle-time)
    (expect 'idle-canary :to-have-been-called))

  (it "should simulate the appropriate value for `(current-idle-time)'"
    (spy-on 'current-idle-time@simulate-idle-time :and-call-through)
    (run-with-idle-timer 1 nil 'idle-canary)
    (wsi-simulate-idle-time 2)
    (expect 'current-idle-time@simulate-idle-time :to-have-been-called)
    (expect canary-idle-time :to-be-truthy)
    (expect (time-equal-p canary-idle-time (seconds-to-time 1))))

  (it "should actually wait the specified time when `actually-wait' is non-nil"
    (spy-on 'sleep-for :and-call-through)
    (run-with-idle-timer 0.01 nil 'idle-canary)
    (run-with-idle-timer 0.02 nil 'idle-canary)
    (run-with-idle-timer 0.03 nil 'idle-canary)
    (run-with-idle-timer 0.04 nil 'idle-canary)
    ;; These shouldn't get called
    (run-with-idle-timer 1 nil 'idle-canary)
    (run-with-idle-timer 2 nil 'idle-canary)
    (run-with-idle-timer 3 nil 'idle-canary)
    (wsi-simulate-idle-time 0.05 t)
    (expect 'idle-canary :to-have-been-called-times 4)
    (expect 'sleep-for :to-have-been-called-times 5))

  (describe "used within `with-simulated-input'"
    (it "should allow idle timers to trigger during simulated input"
      (run-with-idle-timer 500 nil 'insert "world")
      (expect
       (with-simulated-input
           '("hello SPC"
             (wsi-simulate-idle-time 501)
             "RET")
         (read-string "Enter a string: "))
       :to-equal "hello world"))))

;;; test-with-simulated-input.el ends here
