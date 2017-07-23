# with-simulated-input

[![Build Status](https://travis-ci.org/DarwinAwardWinner/with-simulated-input.svg?branch=master)](https://travis-ci.org/DarwinAwardWinner/with-simulated-input)
<!-- (Not in MELPA yet) [![MELPA Stable](https://stable.melpa.org/packages/with-simulated-input-badge.svg)](https://stable.melpa.org/#/with-simulated-input) -->

This package provides a single Emacs Lisp macro,
`with-simulated-input`, which evaluates one or more forms while
simulating a sequence of input events for those forms to read. The
result is the same as if you had evaluated the forms and then manually
typed in the same input. This macro is useful for non-interactive
testing of normally interactive commands and functions, such as
`completing-read`.

For example:

```elisp
(with-simulated-input
    "hello SPC world RET"
  (read-string "Say hello: "))
```

This would return the string `"hello world"`.

<!-- Get it from MELPA: https://stable.melpa.org/#/with-simulated-input -->

## Running the tests

This package comes with a test suite. If you want to run it yourself,
first install the [cask](http://cask.readthedocs.io/en/latest/)
dependency manager. Then, from the package directory, run `cask
install` to install all the development dependencies, in
particular
[buttercup](https://github.com/jorgenschaefer/emacs-buttercup).
Finally, to run the tests, execute `cask exec buttercup -L .`. You
should see something like this:

```
$ cask exec buttercup -L .
Running 17 specs.

`with-simulated-input'
  should work for basic string input
  should throw an error if the input is incomplete
  should allow the input to trigger errors
  should ignore extra input after BODY has completed
  should allow multiple functions in BODY to read input
  should allow aborting via C-g in KEYS
  used with `completing-read'
    should work with unambiguous tab completion
    should work with ambiguous tab completion
    should fail to exit with ambiguous completion and `require-match'Making completion list...

    should fail to exit with ambiguous completion and `require-match'
  using lisp forms in KEYS argument of `with-simulated-input'
    should allow evaluating arbitrary lisp forms
    should allow lisp forms to throw errors
    should not interpret lisp forms once BODY has finished

`wsi-simulate-idle-time'
  should run idle timers
  should not run idle times with longer times
  should run idle timers added by other idle timers
  should run idle timers added by other idle timers when the new timer is in the past
  used within `with-simulated-input'
    should allow idle timers to trigger during simulated input

Ran 17 specs, 0 failed, in 0.6 seconds.
````

Please run the tests before submitting any pull requests, and note in
the pull request whether any of the tests fail.
