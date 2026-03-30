(defpackage :getopt
            (:use :cl)
            (:export :getopt :optind))

(in-package :getopt)

(defvar optind 1)

;; This returns a list, where the first element is either T or NIL. If we have
;; to consume the next element of argv, it's t. That's -X<space>otparg.
;; If we don't consume the next element of argv, it's nil. That's -Xoptarg with
;; no space.
(defun get-optarg (argv argind strind)
  (let ((arg (nth argind argv)))
    (if (= strind (1- (length arg)))
        (cons t (nth (1+ argind) argv))
      (cons nil (subseq arg (1+ strind))))))

;; Given the getopt string, like "abc:d", and the current position within that
;; string, like 2 ("c"), check if the next character is a :. If it is, we expect
;; an argument.
(defun need-optarg (str pos)
  (and (< pos (1- (length str)))
       (char= (char str (1+ pos)) #\:)))

(defun parse-getopt (argv str)
  (let* ((opts '())
        (argc (length argv))
        (opt-index argc))
    ;; skip the first element of argv, which is the program name
    (loop with argind = 1
          while (< argind argc) do
          (let ((arg (nth argind argv)))
            ;; If we encounter "--" as an argument, ignore the rest of the args.
            ;; opt-index points to the next argument.
            (when (string= arg "--")
              (setf opt-index (1+ argind))
              (return))

            ;; - also means stop, but opt-index points to it.
            (when (string= arg "-")
              (setf opt-index argind)
              (return))
            
            ;; If the opt doesn't start with a -, stop parsing arguments here.
            (unless (char= (char arg 0) #\-)
              (setf opt-index argind)
              (return))

            ;; skip the first char, which is -
            (loop for i from 1 below (length arg) do
                  (let ((pos (position (char arg i) str)))
                    (if pos
                        ;; Matching option found!
                        (progn
                          (if (need-optarg str pos)
                              (let ((optarg (get-optarg argv argind i)))
                                ;; if cdr optarg is nil, we don't need to worry
                                ;; if get-optarg consumed the rest of the arg
                                ;; or if it consumed the next element in argv
                                (if (cdr optarg)
                                    (progn
                                      (push (cons (char arg i)
                                                  (cdr optarg))
                                            opts)
                                      ;; if car optarg is t, we consumed the next
                                      ;; element of argv, so we have to skip it
                                      (when (car optarg)
                                        (incf argind))
                                      (return))

                                  (format t "~A: option requires an argument -- ~A~%"
                                          (car argv)
                                          (char arg i))))
                            (push (cons (char arg i)
                                        nil)
                                  opts)))
                      ;; Oops, not a real option! We still push it onto opts
                      ;; so that it can be handled in the getopt call as an
                      ;; unknown option, with t or otherwise.
                      (progn
                        (format t "~A: illegal option -- ~A~%"
                                (car argv)
                                (char arg i))
                        (push (cons (char arg i) nil) opts)))))
            (incf argind)))

    (setf opts (nreverse opts))
    (values opts opt-index)))

;;
;; Usage:
;; (getopt argv "ab:C"
;;   (#\a (do something here...))
;;   (#\b (do something here, utilizizng `optarg`...))
;;   (#\C (do something here...)))
;; `optarg` will be set if the current option has an argument. Otherwise, it
;; will be nil. `getopt:optind` will be set to the last argument processsed, for
;; example, if a command run as such: `./prog -a file.txt`, `optind` will be set
;; to the index of "file.txt".
;;
(defmacro getopt (argv optstr &body clauses)
  (let ((events-var (gensym))
        (ev-var (gensym))
        (optind-var (gensym)))
    (let ((case-clauses
           (mapcar
            (lambda (clause)
              (destructuring-bind (name &body body) clause
                                  `(,name ,@body)))
            clauses)))
      `(multiple-value-bind (,events-var ,optind-var)
                            (parse-getopt ,argv ,optstr)
         (setf getopt:optind ,optind-var)
         (dolist (,ev-var ,events-var)
           (let ((opt (car ,ev-var))
                 (optarg (cdr ,ev-var)))
             (declare (ignorable opt))
             (declare (ignorable optarg))
             (case opt ,@case-clauses)))))))
