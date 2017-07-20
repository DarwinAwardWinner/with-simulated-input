;;; with-simulated-input.el --- A macro to simulate user input non-interactively -*- lexical-binding: t -*-

;; Copyright (C) 2017 Ryan C. Thompson

;; Filename: with-simulated-input.el
;; Author: Ryan C. Thompson
;; Created: Thu Jul 20 11:56:23 2017 (-0700)
;; Version: 1.0
;; Package-Requires: ((emacs "24.4") (seq "0"))
;; URL: 
;; Keywords: 

;; This file is NOT part of GNU Emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 

;; This package provides a single macro, `with-simulated-input', which
;; evaluates one or more forms while simulating a sequence of input
;; events for those forms to read. The result is the same as if you
;; had evaluated the forms and then manually typed in the same input.
;; This macro is useful for non-interactive testing of normally
;; interactive commands and functions, such as `completing-read'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'cl-lib)
(require 'seq)

;;;###autoload
(defmacro with-simulated-input (keys &rest body)
  "Eval BODY forms with KEYS as simulated input.

This macro is intended for automated testing of normally
interactive functions by simulating input. If BODY tries to read
user input (e.g. via `completing-read'), it will read input
events from KEYS instead, as if the user had manually typed those
keys after initiating evaluation of BODY.

If BODY tries to read more input events than KEYS provides, an
error is signalled. This is to ensure that BODY will never block
waiting for input, since this macro is intended for
non-interactive use. If BODY does not consume all the input
events in KEYS, the remaining input events are discarded.

The return value is the last form in BODY, as if it was wrapped
in `progn'."
  (declare (indent 1))
  (let ((temp-cmd (cl-gensym "temp-cmd"))
        (cmd-finished-tag (cl-gensym "cmd-finished"))
        (canary-sym (cl-gensym "canary")))
    `(cl-letf*
         (;; Wrap BODY in a command that evaluates BODY and throws the
          ;; result with `cmd-finished-tag'.
          ((symbol-function ',temp-cmd)
           (lambda ()
             (interactive)
             (throw ',cmd-finished-tag (progn ,@body))))
          ;; Set up the keymap for invoking the temp command
          (transient-map (make-sparse-keymap))
          (command-invoke-key-sequence "C-c e")
          (simulated-key-sequence ,keys)
          (trailing-C-g-key-sequence
           ;; We *really* want to trigger `keyboard-quit' if we reach
           ;; the end of KEYS.
           "C-g C-g C-g C-g C-g C-g C-g")
          (full-key-sequence
           (mapconcat #'identity
                      (list
                       command-invoke-key-sequence
                       simulated-key-sequence
                       trailing-C-g-key-sequence)
                      " ")))
       (when (seq-contains (kbd simulated-key-sequence) (elt (kbd "C-g") 0))
         (error "KEYS must not include C-g"))
       ;; Finish setting up the keymap for the temp command
       (define-key transient-map (kbd command-invoke-key-sequence) ',temp-cmd)
       (set-transient-map transient-map)
       ;; Run the command followed by KEYS followed by C-g. The
       ;; `catch' ensures that the keyboard macro stops executing as
       ;; soon as BODY has finished evaluating, even if there are more
       ;; keys to interpret.
       (let ((result
              (condition-case err
                  (catch ',cmd-finished-tag
                    (execute-kbd-macro (kbd full-key-sequence))
                    ;; If the above doesn't throw, return the canary
                    ',canary-sym)
                ;; On `keyboard-quit', return canary
                (quit ',canary-sym))))
         (if (eq result ',canary-sym)
             (error "Reached end of simulated input while evaluating body")
           result)))))

(provide 'with-simulated-input)

;;; with-simulated-input.el ends here
