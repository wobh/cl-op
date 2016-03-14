cl-op is a partial application and partial evaluation library for Common Lisp inspired by [Goo's op function](http://people.csail.mit.edu/jrb/goo/manual.46/goomanual_15.html#17) and  [SRFI 26 (Notation for Specializing Parameters without Currying)](http://srfi.schemers.org/srfi-26/srfi-26.html).

# Release notes #
  * 0.7.0: Rewrote code walker in CPS.

> Better lifting from special forms.

> Added compiler macro for beta reduction.

> BREAKING CHANGE -- renamed CL-OP-HOF to CL-OP.HOF

  * 0.6.1: Added GENERATOR to CL-OP-HOF.

> Refactored WALK.

  * 0.6.0: Better invariant lifting of macro forms containing slots.

> Added `compose`.

> BREAKING CHANGE: Moved the utility higher order functions (`flip`, `conjoin`, `disjoin`, `compose`) to a seperate package `cl-op-hof`.

  * 0.5.2: Major refactoring.

> Added `conjoin`.

> Exported `conjoin` and `disjoin`.

  * 0.5.1: Made environment passing explicit.

> Added CHANGELOG.txt

  * 0.5.0: BREAKING CHNAGE -- removed `papply`, `papply*`, `pfuncall`, `pfuncall*`, `pmultiple-value-bind`, `pmultiple-value-bind*`, `*walker-ignore-list*`

  * 0.4.2: added support for nested `op`s.

  * 0.4.0: made invariant lifting less aggressive but more robust.

  * 0.3.2: minor cleanup.

  * 0.3.1: bugfix: improper handling of lambda macro.

  * 0.3.0: macroexpanding only to determine whether to lift.

  * 0.2.0: BREAKING CHANGE -- `op` now takes a function name or a lambda expression as the function argument (instead of a function designator as before).

> Added `pfuncall` and `pmultiple-value-call`.

  * 0.1.0: initial release.

# Installation #

cl-op is [ASDF installable](http://www.cliki.net/cl-op).

# Documentation #

## Package cl-op ##

## Macro op, op`*` ##

### Syntax: ###

```
op fn &rest args+ => function
op* fn &rest args+ => function
```

### Description: ###

`op` creates an anonymous function with implicitly defined arguments.

fn is either a function name or a lambda expression (not a function designator. In this regard op is more akin to `function` than `funcall`).  Each _arg_ is either a slot for an implicit required parameter `_` or rest parameter `__` or an s-expression potentially containing further slots or `op` forms (see below). Non-slot args are evaluated.

`op*` is like op except it defers evaluation of non-slot arguments. It is primarily intended to be used with functions with side-effects.

### Examples: ###
```
(mapcar (op / _ 2) '(1 2 4 8 10)) => '(1/2 1 2 4 5)

(mapcar (op / (+ _ _)) '(2 3 4) '(2 3 4)) => '(1/4 1/6 1/8)


(mapcar (op + _ (random 100)) '(1 1 1 1 1)) => '(26 26 26 26 26)
(mapcar (op* + _ (random 100)) '(1 1 1 1 1)) => '(51 51 89 13 99) 
```

### Notes: ###

`op` forms can be meaningfully nested. Slots in nested `op` forms are not added to the surrounding `op` argument list.

Symbols `_` and `__` can still be used as arguments if quoted:
```
(defun __ (x y)
  (equal x y))

(mapcar (op subst _ '_ _ :test #'__) '(a b c) '((foo _ bar _ _ baz) (_ _) (1 2 _ d))) => '((FOO A BAR A A BAZ) (B B) (1 2 C D))
```

Function argument as slot can be simulated by using `funcall` or `apply`:
```
(mapcar (op funcall _ '(1 2 3 4 5)) (list #'first (op nth 2 _))) => '(1 3)
```

## Package cl-op-hof ##

## Function conjoin ##

### Syntax: ###

```
conjoin &rest predicates => function
```

### Description: ###

`conjoin` combines _predicates_ by (logical) and.


## Function disjoin ##

### Syntax: ###

```
disjoin &rest predicates => function
```

### Description: ###

`disjoin ` combines _predicates_ by (logical) or.

## Function compose ##

### Syntax: ###

```
compose &rest functions => function
```

### Description: ###

`compose` composes _functions_.

## Function flip ##

### Syntax: ###

```
flip fn => function
```

### Description: ###

`flip` switches the first two arguments of _fn_.

Among other things it somewhat compensates for `op`'s strict left-to-right argument order.

### Examples: ###

```
(mapcar (flip #'/) '(2 2 2) (1 2 3)) => '(1/2 1 3/2)
```

## Macro generator ##

### Syntax: ###

```
generator &body body => function
```

### Description: ###

`generator` makes an anonymous function with body _body_ that takes any number of arguments (but ignores them).

### Examples: ###

```
(mapcar (generator (1- (random 2.0))) '(2 2 2)) => '(-0.864053 0.52210474 -0.8798127)
```

## Deprecated ##

## `[`deprecated`]` Macro papply, papply`*`, pfuncall, pfuncall`*`, pmultiple-value-call, pmultiple-value-call`*` ##

### Syntax: ###

```
papply &rest args+ => function
papply* &rest args+ => function

pfuncall &rest args+ => function
pfuncall* &rest args+ => function

pmultiple-value-call &rest args+ => function
pmultiple-value-call* &rest args+ => function
```

### Description: ###

`papply`, `pfuncall` and `pmultiple-value-call` are op equivalents of `apply`, `funcall` and `multiple-value-call` respectively.

## `[`deprecated`]` Special variable `*`walker-ignore-list`*` ##

### Description: ###

Parser ignores forms starting with a symbol from this list. Used in situations where `_` or `__` already have an established meaning (e.g. to facilitate `op` nesting).

# Related work #

  * [curly](http://www.cliki.net/curly)
  * [Alexandria](http://common-lisp.net/project/alexandria/) (curry and rcurry)
  * [arnesi](http://common-lisp.net/project/bese/docs/arnesi/html/A_0020reader_0020macro_0020for_0020simple_0020lambdas.html) (L# -- A reader macro for simple lambdas )
  * [F-underscore](http://groups.google.com/group/cl-terrace/web/f-underscore)