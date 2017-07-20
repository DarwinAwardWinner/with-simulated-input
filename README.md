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
particular [ert-runner](https://github.com/rejeep/ert-runner.el).
Finally, to run the tests, execute `cask exec ert-runner`. You should
see something like this:

```
$ cask exec ert-runner
.

Ran 1 test in 0.015 seconds
````

Please run the tests before submitting any pull requests, and note in
the pull request whether any of the tests fail.
