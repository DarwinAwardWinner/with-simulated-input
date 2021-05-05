;;; -*- lexical-binding: t -*-

(require 'buttercup)

(require 'with-simulated-input)

(defun has-advice (symbol advice)
  (let ((advice-fun-to-find
         (indirect-function advice))
        (found nil))
    (advice-mapc
     (lambda (ad-fun ad-props)
       (setq found
             (or found
                 (equal (indirect-function ad-fun)
                        advice-fun-to-find))))
     symbol)
    found))

(describe "The `with-simulated-input' library"

  ;; Run each test with the library unloaded. Obviously this is not
  ;; ideal since we are testing the unloading functionality, but
  ;; there's not much else we can do. We reload the library after each
  ;; test in order to restore the prior state.
  (before-each
    (when (featurep 'with-simulated-input)
      (unload-feature 'with-simulated-input t)))
  (after-each
    (require 'with-simulated-input))

  (it "should be able to load"
    (expect (require 'with-simulated-input)
            :not :to-throw))

  (it "should apply the idle time advice when loading"
    (require 'with-simulated-input)
    (expect (has-advice #'current-idle-time 'current-idle-time@simulate-idle-time)
            :to-be-truthy)
    (spy-on 'current-idle-time@simulate-idle-time :and-call-through)
    (current-idle-time)
    (expect 'current-idle-time@simulate-idle-time
            :to-have-been-called))

  (it "should be able to unload"
    ;; Load and unload 3 times, just to make sure there aren't errors
    ;; on subsequent reloadings.
    (expect (require 'with-simulated-input)
            :not :to-throw)
    (expect (featurep 'with-simulated-input))
    (expect (unload-feature 'with-simulated-input t)
            :not :to-throw)
    (expect (not (featurep 'with-simulated-input)))
    (expect (require 'with-simulated-input)
            :not :to-throw)
    (expect (featurep 'with-simulated-input))
    (expect (unload-feature 'with-simulated-input t)
            :not :to-throw)
    (expect (not (featurep 'with-simulated-input)))
    (expect (require 'with-simulated-input)
            :not :to-throw)
    (expect (featurep 'with-simulated-input))
    (expect (unload-feature 'with-simulated-input t)
            :not :to-throw)
    (expect (not (featurep 'with-simulated-input))))

  (it "should remove the idle time advice when unloading"
    (expect (require 'with-simulated-input)
            :not :to-throw)
    (expect (has-advice #'current-idle-time 'current-idle-time@simulate-idle-time)
            :to-be-truthy)
    (expect (unload-feature 'with-simulated-input t)
            :not :to-throw)
    (expect (has-advice #'current-idle-time 'current-idle-time@simulate-idle-time)
            :not :to-be-truthy)))
;;; test-unload.el ends here
