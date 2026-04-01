
Getopt
==================

**Simple getopt implementation for Common Lisp**


Introduction
------------

This is a simple POSIX-style implementation of `getopt` in Common Lisp. `(getopt)`  is a macro that allows command line parsing that is easy to read, write, and understand, while still implementing all the features of GNU's `getopt` library. 


Examples
--------

Example command line:

    $ ./program -d3 -f one -xyz foo -uvwfoo -v -- -v

That command line could be parsed with the following macro call:

```cl
(let ((argv sb-ext:*posix-argv*))
  (getopt argv "d:f:xyz:uvw:"
   (#\x
     (do-stuff))
   ((#\y #\u #\v)
     (do-more-stuff getopt:opt))
   ((#\d #\f #\z #\w)
 (do-more-stuff-2.0 getopt:opt getopt:optarg))))
```
The macro expands to a `cond`, so you can match multiple arguments in one conditional by matching a list.

You can also handle unknown options, or missing arguments with `#\?` and `#\:`, like this:
```cl
(getopt argv "a:"
  (#\a
    (format t "Argument: ~A~%" getopt:optarg))
  (#\?
    (format t "Woah, ~A is an unknown argument!~%" getopt:optopt))
  (#\:
    (format t "Oops, ~A needs an argument!~%" getopt:optopt)))
```
If you don't want `getopt` to print its own warning messages, like `"./prog: illegal option -- z`, you can use the `:no-warn` keyword after the optstring:
`(getopt argv "abc" :no-warn ...)`
and then all warning messages will be silenced. You can also use a leading `:` in the optstring to achieve the same effect:
`(getopt argv ":abc" ...)`

You can also use `t` or `otherwise` as a catch-all for unhandled options:
```cl
(getopt argv "a:" :no-warn
  (#\a (do-stuff))
  (otherwise
    (handle-other-stuff-here)))
```
The body of each match is not limited to one expression:
```cl
(getopt argv "a:b" :no-warn
  (#\a
    (when (string= getopt:optarg "foo")
      (format t "foo?!~%"))
    (format t "Is that so?!~%")
    (format t "You're really going with ~A, huh...~%" getopt:optarg))
  (#\b
    (format t "That's fine, I guess.~%"))
  (otherwise
    (format t "Whatever.~%")))
```

Programming Interface
-------------------------
**Macro:** `(getopt (argv optstr &body clauses))`

Parses `argv` according to the options outlined in `optstr`.


**In-macro variable:** `getopt:opt`

This is the character representing the current option. If you're parsing options and match `-a`, then `getopt:opt` will be `#\a`. If an error occurs, such as an invalid option or missing argument, `getopt:opt` will be either `#\?` or `#\:` respectively.


**In-macro variable:** `getopt:optarg`

If the current option takes an argument, that argument is stored in `getopt:optarg`. If the current option does not take an argument, or the argument is missing, then this is `nil`. 


**Global variable:** `getopt:*optind*`

This behaves the same as `optind` in C's getopt. `getopt:*optind*` is the index of the *next* element in `argv` to be processed. See the [OpenGroup Specification for getopt](https://pubs.opengroup.org/onlinepubs/009696799/functions/getopt.html) for more.


**In-macro variable:** `getopt:optopt`

This is the character representing the current option, similar to `getopt:opt`. However, it's useful when processing `#\?` and `#\:`. In those cases, `getopt:opt` is either `#\?` or `#\:`, while `getopt:optopt` is the literal character that caused the error.


All of these variables, with the exception of `getopt:*optind*`, can only be accessed within the body of the `(getopt ...)` call. For example:
```cl
(getopt argv "abc"
  (#\a
    (format t "~A ~A ~A ~A~%"
      getopt:opt getopt:optarg
      getopt:optopt getopt:*optind*)))
```
Is fine, but
```cl
(getopt ...)
(format t "~A" getopt:opt)
```
*will* cause a problem. However, `getopt:*optind*` can be accessed from outside of the macro body.

Usage
-----
You can use this with `asdf`. Put `getopt.lisp` and `getopt.asd` in your ASDF source dir. I think it's typically `~/.local/share/common-lisp/source`. Then, include this in your code when you'd like to use getopt:
```cl
(require "asdf")
(asdf:load-system "getopt")
```
Or, if you want to use `(getopt ...)` instead of `(getopt:getopt ...)`, you can add this, too:
```
(import 'getopt:getopt)
````
