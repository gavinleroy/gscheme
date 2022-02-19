# GScheme

GScheme is a small scheme implementation that will grow over a time eventually reaching a near implementation of the [Revised 6th Report of the Algorithmic Language Scheme](http://www.r6rs.org/). 

Currently, GScheme is a very small subset of this goal using a macro expander inspired by Matthew Flatt's [*Let's Build a Hygenic Macro Expander*](https://www.youtube.com/watch?v=Or_yKiI3Ha4) talk at Strange Loop 2016 [[1]](#1).

### State of the expander

- [X] pico : expander with lambda calculus
- [X] nano : implicit quoting, multi-arg lambdas
- [X] micro : custom matcher used internally, identifier macros allowed, expands application to an `#%app` form. 
- [ ] mini 
- [ ] demi

## Various Details

A warning note on the style of the source code: despite being written in OCaml, the code currently follows a slight Scheme style of implementation and standard OCaml best practices are not always followed. I suspect that this will change over time but in such an early stage no promises are made.

### Commands

To aid with debugging and visualization one can set the REPL to only expand the expression and display the result, thus skipping evaluation. 
This is set using the command syntax: `,cfg (eval #f)` and reenabled with the same but using `#t`.
Further commands outside of `,cfg` will be introduced at a later stage for debugging and inspecting continuations.

### Examples

Short examples can be typed into the REPL, however, top-level bindings (e.g. `(define id (lambda (x) x))`) are not currently supported in the expander and will produce a *bad syntax* error. Here are some brief examples of what one may currently write:

``` scheme
> (lambda (x) (+ 1 x))
  => #<procedure anonymous>
  
> (map (lambda (x) (* x x)) '(1 2 3 4 5))
  => (1 4 9 16 25)
```

A slightly larger example:

``` scheme
> (let-syntax ((let (lambda (stx)
                      (datum->syntax
                       (quote-syntax here)
                       (cons
                        (list (quote-syntax lambda)
                              (map (lambda (b)
                                     (car (syntax-e b)))
                                   (syntax-e (car (cdr (syntax-e stx)))))
                              (car (cdr (cdr (syntax-e stx)))))
                        (map (lambda (b)
                               (car (cdr (syntax-e b))))
                             (syntax-e (car (cdr (syntax-e stx))))))))))
    (let ((z 9))
               (let-syntax ((m (lambda (stx) (cadr (syntax-e stx)))))
                 (let ((x 5)
                       (y (lambda (z) z)))
                   (let ((z 10))
                     (list z (m 10)))))))
  => (10 10)
```

Sadly, the REPL will not *currently* accept newlines in the expression so the above might be difficult to type all on one line (:

## Progress

GScheme progress is slightly (and will continue to be) staggered. Largly influenced by free time in between semesters and homework. 
In general, the underlying scheme implementation provides functionality as needed by the macro expander. Once macros are in a sufficient state, the rest of the evaluation and standard library will be added.

## Reference Links

<a id="1"> [1] </a> <github.com/mflatt/expander>
