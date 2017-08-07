;;; with-simulated-input.el --- A macro to simulate user input non-interactively -*- lexical-binding: t -*-

;; Copyright (C) 2017 Ryan C. Thompson

;; Filename: with-simulated-input.el
;; Author: Ryan C. Thompson
;; Created: Thu Jul 20 11:56:23 2017 (-0700)
;; Version: 2.2
;; Package-Requires: ((emacs "24.4") (seq "2.0") (s "0"))
;; URL:
;; Keywords: lisp, tools, extensions

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
(require 's)

(cl-defun wsi-key-bound-p (key)
  "Return non-nil if KEY is bound in any keymap.

This function checks every keymap in `obarray' for a binding for
KEY, and returns t if it finds and and nil otherwise. Note that
this checks ALL keymaps, not just currently active ones."
  (catch 'bound
    (mapatoms
     (lambda (sym)
       (let ((keymap
              (when (boundp sym)
                (symbol-value sym))))
         (when (keymapp keymap)
           (let ((binding (lookup-key keymap (kbd key))))
             (when binding
               (throw 'bound t)))))))
    (throw 'bound nil)))

(cl-defun wsi-get-unbound-key
    (&optional (modifiers '("C-M-A-s-H-" "C-M-A-s-" "C-M-A-H-"))
               (keys "abcdefghijklmnopqrstuvwxyz0123456789"))
  "Return a key binding that is not bound in any known keymap.

This function will try check every letter from a to z and every
number from 0 through 9 with several combinations of multiple
modifiers (i.e. control, meta, alt, super, hyper). For each such
key combination, it will check for bindings in all known keymaps,
and return the first combination for which no such bindings
exist. Thus, it should be safe to bind this key in a new keymap
without interfering with any existing keymap.

Optional arguments MODIFIERS and KEYS can be used the change the
search space. MODIFIERS is a list of strings representing
modifier combinations, e.g.:

    '(\"C-\" \"M-\" \"C-M-\")

for control, meta, or both. KEYS is a string containing all keys
to check.
"
  (declare (advertised-calling-convention (&optional modifiers keys) nil))
  (when (stringp modifiers)
    (setq modifiers (list modifiers)))
  (when (listp keys)
    (setq keys (apply #'concat keys)))
  (cl-loop
   named findkey
   for modifier in modifiers
   do (cl-loop
       for char across keys
       for bind = (concat modifier (string char))
       when (not (wsi-key-bound-p bind))
       do (cl-return-from findkey bind))
   finally do (error "Could not find an unbound key with the specified modifiers")))

(defmacro wsi-current-lexical-environment ()
  "Return the current lexical environment.

This macro expands to a lisp form that evaluates to the current
lexical environment. It works by creating a closure and then
extracting and returning its lexical environment.

This can be used to manually construct closures in that
environment."
  `(let ((temp-closure (lambda () t)))
     (cl-assert (eq (car temp-closure) 'closure) t)
     (cadr temp-closure)))

(defun wsi-make-closure (expr env)
  `(closure ,env () ,expr))

;;;###autoload
(defmacro with-simulated-input (keys &rest body)
  "Eval BODY forms with KEYS as simulated input.

This macro is intended for automated testing of normally
interactive functions by simulating input. If BODY tries to read
user input (e.g. via `completing-read'), it will read input
events from KEYS instead, as if the user had manually typed those
keys after initiating evaluation of BODY.

KEYS should be a string representing a sequence of key presses,
in the format understood by `kbd'. In the most common case of
typing in some text and pressing RET, KEYS would be something
like `\"hello RET\"'. Note that spaced must be indicated
explicitly using `SPC', e.g. `\"hello SPC world RET\"'.

KEYS can also be a list. In this case, each element should either
be a key sequence as described above or an arbitrary lisp form
that will be evaluated at that point in the input sequence. For
example, `\"hello RET\"' could also be written as:

    `((insert \"hello\") \"RET\")'

If BODY tries to read more input events than KEYS provides, an
error is signalled. This is to ensure that BODY will never block
waiting for input, since this macro is intended for
non-interactive use. If BODY does not consume all the input
events in KEYS, the remaining input events in KEYS are discarded,
and any remaining lisp forms in KEYS are never evaluated.

Any errors generated by any means during the evaluation of BODY
are propagated normally.

The return value is the last form in BODY, as if it was wrapped
in `progn'."
  (declare (indent 1))
  `(cl-letf*
       ((lexenv (wsi-current-lexical-environment))
        (next-action-key (wsi-get-unbound-key))
        (canary-sym ',(cl-gensym "wsi-canary-"))
        (result canary-sym)
        (thrown-error nil)
        (body-form
         '(throw 'wsi-body-finished (progn ,@body)))
        (end-of-actions-form
         (list 'throw
               '(quote wsi-body-finished)
               (list 'quote canary-sym)))
        ;; Ensure KEYS is a list, and put the body form as the first
        ;; item and `C-g' as the last item
        (keylist ,keys)
        (keylist (if (listp keylist)
                     keylist
                   (list keylist)))
        ;; Replace non-strings with `next-action-key' and concat
        ;; everything together
        (full-key-sequence
         (cl-loop
          for action in keylist
          if (stringp action)
          collect action into key-sequence-list
          else
          collect next-action-key into key-sequence-list
          finally return
          ;; Prepend and append `next-action-key' to run body and canary
          (concat
           next-action-key " "
           (mapconcat #'identity key-sequence-list " ")
           " " next-action-key)))
        ;; Extract non-string forms, adding body at the front and
        ;; canary at the back
        (action-list
         (nconc
          (list body-form)
          (cl-loop
           for action in keylist
           if (not (stringp action))
           collect action)
          (list end-of-actions-form)))
        ;; Wrap each action in a lexical closure so it can refer to
        ;; variables from the caller.
        (action-closures
         (cl-loop
          for action in action-list
          collect (wsi-make-closure action lexenv)))
        ;; Define the next action command with lexical scope so it can
        ;; access `action-closures'.
        ((symbol-function 'wsi-run-next-action)
         (lambda ()
           (interactive)
           (condition-case err
               (if action-closures
                   (let ((next-action (pop action-closures)))
                     (funcall next-action))
                 (error "`with-simulated-input' reached end of action list without returning."))
             (error (throw 'wsi-threw-error err)))))
        ;; Set up the temporary keymap
        (action-map (make-sparse-keymap)))
     ;; Finish setting up the keymap for the temp command
     (define-key action-map (kbd next-action-key) 'wsi-run-next-action)
     (setq
      thrown-error
      (catch 'wsi-threw-error
        (setq
         result
         (catch 'wsi-body-finished
           (let ((overriding-terminal-local-map action-map))
             (execute-kbd-macro (kbd full-key-sequence)))))
        ;; If we got here, then no error
        (throw 'wsi-threw-error nil)))
     (when thrown-error
       (signal (car thrown-error) (cdr thrown-error)))
     (if (eq result canary-sym)
         (error "Reached end of simulated input while evaluating body")
       result)))

(defvar wsi-simulated-idle-time nil)

(defadvice current-idle-time (around simulate-idle-time activate)
  "Return the faked value while simulating idle time.

While executing `wsi-simulate-idle-time', this advice causes the
simulated idle time to be returned instead of the real value."
  (if wsi-simulated-idle-time
      (setq ad-return-value
            (when (time-less-p (seconds-to-time 0) wsi-simulated-idle-time)
              wsi-simulated-idle-time))
    ad-do-it))

(cl-defun wsi-simulate-idle-time (&optional secs actually-wait)
  "Run all idle timers with delay less than SECS.

This simulates resetting the idle time to zero and then being
idle for SECS seconds. Hence calling this function twice with
SECS = 1 is not equivalent to 2 seconds of idle time.

If ACTUALLY-WAIT is non-nil, this function will also wait for the
specified amount of time before running each timer.

If SECS is nil, simulate enough idle time to run each timer in
`timer-idle-list' at least once. (It's possible that some timers
will be run more than once, since each timer could potentially
add new timers to the list.)

While each timer is running, `current-idle-time' will be
overridden to return the current simulated idle time.

This function does not run any timers in `timer-list', even
though they would run during real idle time."
  (interactive
   "nSeconds of idle time: \nP")
  ;; SECS defaults to the maximum idle time of any currently active
  ;; timer.
  (unless secs
    (setq secs
          (cl-loop for timer in timer-idle-list
                   maximize (float-time (timer--time timer)))))
  ;; Add a small fudge factor to deal with SECS being exactly equal to
  ;; a timer's time, to avoid floating point issues.
  (setq secs (+ secs 0.0001))
  (cl-loop
   with already-run-timers = nil
   with stop-time = (seconds-to-time secs)
   with wsi-simulated-idle-time = (seconds-to-time 0)
   ;; We have to search `timer-idle-list' from the beginning each time
   ;; through the loop because each timer that runs might add more
   ;; timers to the list, and picking up at the same list position
   ;; would ignore those new timers.
   for next-timer = (car (cl-member-if-not
                          (lambda (timer) (memq timer already-run-timers))
                          timer-idle-list))
   while next-timer
   for previous-idle-time = wsi-simulated-idle-time
   if (time-less-p wsi-simulated-idle-time
                   (timer--time next-timer))
   do (setq wsi-simulated-idle-time
            (timer--time next-timer))
   when actually-wait
   do (sleep-for (float-time (time-subtract wsi-simulated-idle-time
                                            previous-idle-time)))
   while (time-less-p wsi-simulated-idle-time stop-time)
   when (not (timer--triggered next-timer))
   do (timer-event-handler next-timer)
   do (push next-timer already-run-timers)
   finally do
   (when actually-wait
     (sleep-for (float-time (time-subtract stop-time
                                           wsi-simulated-idle-time))))))

(provide 'with-simulated-input)

;;; with-simulated-input.el ends here
