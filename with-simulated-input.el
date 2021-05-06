;;; with-simulated-input.el --- A macro to simulate user input non-interactively -*- lexical-binding: t -*-

;; Copyright (C) 2017 Ryan C. Thompson

;; Filename: with-simulated-input.el
;; Maintainer: Ryan C Thompson <rct@thompsonclan.org>
;; Author: Ryan C. Thompson <rct@thompsonclan.org>
;;    Nikita Bloshchanevich <nikblos@outlook.com>
;; Created: Thu Jul 20 11:56:23 2017 (-0700)
;; Version: 2.4
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/DarwinAwardWinner/with-simulated-input
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

This function will check every letter from a to z and every
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
to check."
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

;;;###autoload
(defun with-simulated-input-1 (main &rest keys)
  "Internal `with-simulated-input' helper.
KEYS is a keylist as can be passed to that function (except that
only a list is allowed, and forms must be functions) and MAIN is
the body form as a function."
  (let* ((next-action-key (wsi-get-unbound-key))
         ;; Ensure we don't interfere with any outside catching.
         (result-sym (make-symbol "with-simulated-input-result"))
         (error-sym (make-symbol "with-simulated-input-error"))
         (orig-buf (current-buffer))
         (actions
          (nconc
           (list (lambda ()
                   (switch-to-buffer orig-buf)
                   (throw result-sym (funcall main))))
           (cl-remove-if-not #'functionp keys)
           (list (lambda ()
                   (error "Reached end of simulated input while evaluating body")))))
         (overriding-terminal-local-map
          (if overriding-terminal-local-map
              (copy-keymap overriding-terminal-local-map)
            (make-sparse-keymap))))
    (define-key overriding-terminal-local-map (kbd next-action-key)
      (lambda ()
        (interactive)
        (condition-case data
            (funcall (pop actions))
          (error (throw error-sym data)))))
    (catch result-sym
      ;; Signals are not passed through `read-from-minibuffer'.
      (let ((err (catch error-sym
                   (execute-kbd-macro
                    (kbd (mapconcat
                          #'identity
                          (nconc (list next-action-key)
                                 (cl-loop for key in keys collect
                                          (if (stringp key) key next-action-key))
                                 (list next-action-key))
                          " "))))))
        (signal (car err) (cdr err))))))

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
be a key sequence as described above or an arbitrary Lisp form
that will be evaluated at that point in the input sequence. For
example, `\"hello RET\"' could also be written as:

    `((insert \"hello\") \"RET\")'

If BODY tries to read more input events than KEYS provides, an
error is signalled. This is to ensure that BODY will never block
waiting for input, since this macro is intended for
non-interactive use. If BODY does not consume all the input
events in KEYS, the remaining input events in KEYS are discarded,
and any remaining Lisp forms in KEYS are never evaluated.

Any errors generated by any means during the evaluation of BODY
are propagated normally.

The return value is the last form in BODY, as if it was wrapped
in `progn'."
  (declare
   (indent 1)
   (debug ([&or ("quote" (&rest &or stringp def-form)) ; quoted list of string-or-form
                (&rest &or stringp def-form) ; un-quoted same
                stringp symbolp         ; literal string; variable name (or nil)
                ([&or functionp macrop] &rest form) ; arbitrary lisp function call
                ]
           def-body)))
  ;; TODO Warn on empty body
  ;; TODO Support integers (i.e. single characters) in KEYS
  (cond
   ((null keys)
    ;; (message "Keys is nil")
    `(with-simulated-input-1
      (lambda ()
        ,@body)
      nil))
   ((and keys (symbolp keys))
    ;; (message "keys is symbol: %S" keys)
    `(progn
       (cond
        ((null ,keys)
         (with-simulated-input-1
          (lambda ()
            ,@body)
          nil))
        ((stringp ,keys)
         (with-simulated-input-1
          (lambda ()
            ,@body)
          ,keys))
        ((listp ,keys)
         (apply
          #'with-simulated-input-1
          (lambda ()
            ,@body)
          (cl-loop for key in ,keys collect (if (stringp key) key `(lambda () ,key)))))
        (t
         (error "INVALID VAR VALUE: %S" ,keys)))))
   ((and (listp keys)
         (not (eq (car keys) 'quote))
         (or (functionp (car keys))
             (macrop (car keys))))
    ;; (message "Keys is lisp form: %S" keys)
    `(let ((evaluated-keys (,@keys)))
       ;; (message "Evaluated keys: %S" evaluated-keys)
       (pcase evaluated-keys
         (`(quote ,x) (setq evaluated-keys x))
         ((guard (not (listp evaluated-keys))) (cl-callf list evaluated-keys)))
       ;; (message "Evaluated keys transformed: %S"
       ;;          (cl-loop for key in evaluated-keys collect (if (stringp key) key `(lambda () ,key))))
       (apply
        #'with-simulated-input-1
        (lambda ()
          ,@body)
        (cl-loop for key in evaluated-keys collect (if (stringp key) key `(lambda () ,key))))))
   (t
    ;; (message "Keys is something else: %S" keys)
    (pcase keys
      (`(quote ,x) (setq keys x))
      ((guard (not (listp keys))) (cl-callf list keys)))
    `(with-simulated-input-1
      (lambda ()
        ,@body)
      ,@(cl-loop for key in keys collect (if (stringp key) key `(lambda () ,key)))))))

(defvar wsi-simulated-idle-time nil
  "The current simulated idle time.

While simulating idle time using `wsi-simulated-idle-time', this
variable will always be set to the amount of idle time that has
been simulated so far. For example, if an idle time is set to run
every 5 seconds while idle, then on its first run, this will be
set to 5 seconds, then 10 seconds the next time, and so on.")

(defun current-idle-time@simulate-idle-time (orig-fun &rest args)
  "Return the faked value while simulating idle time.

While executing `wsi-simulate-idle-time', this advice causes the
simulated idle time to be returned instead of the real value.

ORIG-FUN is the original function, passed by `advice-add'; ARGS
are the arguments given to it."
  (if wsi-simulated-idle-time
      (when (time-less-p (seconds-to-time 0) wsi-simulated-idle-time)
        wsi-simulated-idle-time)
    (apply orig-fun args)))
(advice-add 'current-idle-time
            :around #'current-idle-time@simulate-idle-time)

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

The idle time simulation provided by this function is not
perfect. For example, this function does not run any timers in
`timer-list', even though they would run as normal during real
idle time. In addition, weird effects may occur if idle timers
add other idle timers."
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
                          (lambda (timer)
                            (and (memq timer already-run-timers)))
                          timer-idle-list))
   ;; Stop if we reach the end of the idle timer list, or if the next
   ;; timer's idle time is greater than SECS
   while (and next-timer (time-less-p (timer--time next-timer) stop-time))
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

(defun with-simulated-input-unload-function ()
  "Unload the `with-simulated-input' library."
  (advice-remove 'current-idle-time
                 #'current-idle-time@simulate-idle-time))

(provide 'with-simulated-input)

;;; with-simulated-input.el ends here
