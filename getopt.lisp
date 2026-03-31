(defpackage :getopt
            (:use :cl)
            (:export :getopt :*optind* :opt :optarg))

(in-package :getopt)

(defvar *optind* 1)

;; This returns a list, where the first element is either T or NIL. If we have
;; to consume the next element of argv, it's t. That's -X<space>otparg.
;; If we don't consume the next element of argv, it's nil. That's -Xoptarg with
;; no space.
(defun get-optarg (argv argv-pos char-pos)
  (let ((arg (aref argv argv-pos)))
    (if (= char-pos (1- (length arg)))
        (if (< (1+ argv-pos) (length argv))
            (cons t (aref argv (1+ argv-pos)))
          (cons t nil))
      (cons nil (subseq arg (1+ char-pos))))))

;; Given the getopt string, like "abc:d", and the current position within that
;; string, like 2 ("c"), check if the next character is a :. If it is, we expect
;; an argument.
(defun need-optarg (str pos)
  (and (< pos (1- (length str)))
       (char= (char str (1+ pos)) #\:)))

(defun make-opt (arg optarg argv-pos char-pos)
  (list (char arg char-pos)
        optarg
        (if (= char-pos (1- (length arg)))
            (1+ argv-pos)
          argv-pos)))

(defmacro try-warn (warn-p format-str &rest args)
  `(when ,warn-p
     (format t ,format-str ,@args)))

(defun parse-getopt (argv str warn-p)
  (let* ((opts '())
         (argv (if (vectorp argv)
                   argv
                 (coerce argv 'vector)))
         (argc (length argv))
         (opt-index argc))
    ;; skip the first element of argv, which is the program name
    (loop with argv-pos = 1
          while (< argv-pos argc) do
          (let ((arg (aref argv argv-pos)))
            ;; If we encounter "--" as an argument, ignore the rest of the args.
            ;; opt-index points to the next argument.
            (when (string= arg "--")
              (setf opt-index (1+ argv-pos))
              (return))

            ;; - also means stop, but opt-index points to it.
            (when (string= arg "-")
              (setf opt-index argv-pos)
              (return))
            
            ;; If the opt doesn't start with a -, stop parsing arguments here.
            (unless (char= (char arg 0) #\-)
              (setf opt-index argv-pos)
              (return))

            ;; This loop handles multiple-character opt strings. So, -abcd. This
            ;; skips the first char, which is -a, and then loops over each char
            ;; and processes it as an option.
            (loop for char-pos from 1 below (length arg) do
                  (let ((pos (position (char arg char-pos) str)))
                    (if pos
                        ;; Matching option found!
                        (progn
                          (if (need-optarg str pos)
                              (let ((optarg
                                     (get-optarg argv argv-pos char-pos)))
                                ;; if cdr optarg is nil, we don't need to worry
                                ;; if get-optarg consumed the rest of the arg
                                ;; or if it consumed the next element in argv
                                (if (cdr optarg)
                                    (progn
                                      ;; if car optarg is t, we consumed the
                                      ;; next element of argv, so we have to
                                      ;; skip it.
                                      (when (car optarg)
                                        (incf argv-pos))

                                      (push (make-opt arg
                                                      (cdr optarg)
                                                      argv-pos
                                                      char-pos)
                                            opts)
                                      (return))

                                  (try-warn warn-p
                                   "~A: option requires an argument -- ~A~%"
                                   (aref argv 0)
                                   (char arg char-pos))))
                            (push (make-opt arg nil argv-pos char-pos)
                                  opts)))
                      ;; Oops, not a real option! We still push it onto opts
                      ;; so that it can be handled in the getopt call as an
                      ;; unknown option, with t or otherwise.
                      (progn
                        (try-warn warn-p
                                  "~A: illegal option -- ~A~%"
                                  (aref argv 0)
                                  (char arg char-pos))
                        (push (make-opt arg nil argv-pos char-pos)
                              opts)))))
            (incf argv-pos)))

    (setf opts (nreverse opts))
    (values opts opt-index)))

;;
;; Usage:
;; (getopt argv "ab:C"
;;   (#\a (do something here...))
;;   (#\b (do something here, utilizizng `optarg`...))
;;   (#\C (do something here...)))
;; `optarg` will be set if the current option has an argument. Otherwise, it
;; will be nil. `getopt:*optind*` will be set to the last argument processsed, for
;; example, if a command run as such: `./prog -a file.txt`, `*optind*` will be set
;; to the index of "file.txt".
;;
(defmacro getopt (argv optstr &body clauses)
  ;; If called with (getopt ... :no-warn, we suppress the warning messages for
  ;; missing arguments, etc. This is handled in try-warn. Here, we check if the
  ;; first element of clauses is ":no-warn", and set warn-p to nil if it, or t
  ;; if it's not.
  (let ((warn-p (not (eq (car clauses) :no-warn))))
    (when (eq (car clauses) :no-warn)
      ;; Get rid of :no-warn so it doesn't get processed in the cond below.
      (pop clauses))

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
                              (parse-getopt ,argv ,optstr ,warn-p)
           (setf getopt:*optind* ,optind-var)
           (dolist (,ev-var ,events-var)
             (let ((opt (car ,ev-var))
                   (optarg (cadr ,ev-var))
                   (*optind* (cddr ,ev-var)))
               (declare (ignorable opt))
               (declare (ignorable optarg))
               (case opt ,@case-clauses))))))))
