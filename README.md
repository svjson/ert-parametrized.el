
# ert-params.el

> Parametrized test macros for ert

`ert-params` is a small Emacs Lisp library that adds **parameterized tests** to ERT.  
It provides a `ert-deftest-parametrized` macro similar in spirit to pytest’s parameterized tests and allows:

- Named test cases
- Literal and evaluated parameters (`:literal`, `:eval`)
- Function-parameter injection (`:fun`)
- Generator-based expansion (`:generator`)
- Fully macro-expanded into real ERT tests

This library is currently **experimental (v0.1.0)**

## Features

### Declarative test case tables

Test cases are described as simple tables, or lists of test case forms and supports
both literal and evaluated forms.

```elisp
(ert-deftest-parametrized example-test

    ;; Input variables bound for each case
    (input expected)

    ;; Named cases and inputs
    (("numbers"
      (:literal 40)
      (:literal 42))
     ("expr"
      (:eval (+ 18 22 40))
      (:literal 82)))

  ;; Test body
  (should (= (+ input 2) expected)))
```

This example generates the following `ert-deftest` equivalents:
- `example-test--numbers` with the parameters 40 and 42.
- `example-test--expr` with the parameters 80 and 82

### Function-bound parameters

More complex test may depend on pre-conditions or state that requires code execution,
and this can be achieved by binding forms with `:fun` which generates an fbound parameters
that evaluates these forms.

```elisp
(ert-deftest-parametrized fun-example-test
    (move-into-position! expected)
  
    (("from-buffer-start"
      (:fun (goto-char (point-min))))
     ("from-buffer-end"
      (:fun (goto-char (point-max))))
     ("from-two-lines-down"
      (:fun (progn (goto-char (point-min))
                   (forward-line 2)))))
                   
  (with-temp-buffer 
      ;; ... Buffer setup, etc
      
      (move-into-position!)
      
      ;; ... Perform action to test
      
      (should ...)))
```

### Generators

The real power comes with the ability to generate test case forms from within test case
forms using `:generator` parameters. They may contain anything that evaluates to a list.

Each generated test case form will expand to a separate `ert-deftest` form, which requires
the original test case form to provide placeholders in their name. String interpolation is
performed with `format` and thus follows a syntax that should be familiar to everyone.

```elisp
(ert-deftest-parametrized generator-example
    (input expected)
   
    (("%d-multiplied-by-2-equals-%d"
       (:generator (:eval (number-sequence 0 10)))
       (:generator (:literal (0 2 4 6 8 10 12 14 16 18 20)))))
      
  (should (equal (* input 2)
                 expected)))
```

This generates `ert-deftest` forms for `generator-example--0-multiplied-by-2-equals-0` 
and all the way up to `generator-example--10-multiplied-by-2-equals-20`

`:generator` parameters can be combined with other parameters.


## License

© 2025 Sven Johansson. This package is licensed under the **GNU GPL v3.0**. See [`LICENSE`](LICENSE) for details.
