;;; getopt.lisp -- POSIX-style getopt for Common Lisp
;;; Copyright (C) 2026 imtomt
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, version 3.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program. If not, see <https://www.gnu.org/licenses/>.

(defpackage :getopt
            (:use :cl)
            (:export :getopt :*optind* :opt :optarg :optopt))

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

;; Make the option list that parse-getopt builds. Each option is a list of the
;; form:
;; (opt-character   optarg [or nil if none]   optind   optopt)
;; If arg is a character, like #\? or #\:, the actual argument string should be
;; passed to optopt.
(defun make-opt (arg optarg argv-pos char-pos &optional optopt)
  (let ((actual-opt (cond ((stringp arg) arg)
                          ((stringp optopt) optopt)
                          (t
                           (error "make-opt: neither arg nor optopt is str")))))
    (list
     (if (characterp optopt)
         optopt
       (char arg char-pos))
     optarg
     ;; If we're the last option in the string, optind should point to the next
     ;; option.
     (if (= char-pos (1- (length actual-opt)))
         (1+ argv-pos)
       argv-pos)
     (char actual-opt char-pos))))

(defun try-warn (warn-p format-str &rest args)
  (when warn-p
    (apply #'format *error-output* format-str args)))

(defun parse-getopt (argv str warn-p)
  (let* ((opts '())
         (argv (if (vectorp argv)
                   argv
                 (coerce argv 'vector)))
         (argc (length argv))
         (opt-index argc))

    (when (eq (char str 0) #\:)
      (setf warn-p nil))

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
            ;; skips the first char, which is '-', and then loops over each char
            ;; and processes it as an option.
            (loop for char-pos from 1 below (length arg) do
                  ;; -: is itself an illegal option. So only set pos, indicating
                  ;; a valid match, if ch is not :. This shouldn't affect "ab:c"
                  ;; in the opt string.
                  (let* ((ch (char arg char-pos))
                         (pos (unless (char= ch #\:)
                                (position ch str))))
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

                                  (progn
                                    (try-warn warn-p
                                      "~A: option requires an argument -- ~A~%"
                                      (aref argv 0) ch)

                                    (push (make-opt arg
                                                    nil
                                                    argv-pos
                                                    char-pos
                                                    #\:)
                                          opts))))
                            (push (make-opt arg nil argv-pos char-pos)
                                  opts)))
                      ;; Oops, not a real option! We still push it onto opts
                      ;; so that it can be handled in the getopt call as an
                      ;; unknown option, with t or otherwise.
                      (progn
                        (try-warn warn-p
                                  "~A: illegal option -- ~A~%"
                                  (aref argv 0) ch)
                        (push (make-opt arg nil argv-pos char-pos #\?)
                              opts)))))
            (incf argv-pos)))

    (setf opts (nreverse opts))
    (values opts opt-index)))

;;
;; Usage:
;; (getopt argv "ab:C"
;;   (#\a (do something here...))
;;   (#\b (do something here, utilizing `optarg`...))
;;   (#\C (do something here...)))
;; `optarg` will be set if the current option has an argument. Otherwise, it
;; will be nil. `getopt:*optind*` will be set to the last argument processed,
;; for example, if a command run as such: `./prog -a file.txt`, `*optind*` will
;; be set to the index of "file.txt".
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
             (let ((opt (first ,ev-var))
                   (optarg (second ,ev-var))
                   (*optind* (third ,ev-var))
                   (optopt (fourth ,ev-var)))
               (declare (ignorable opt optarg optopt))
               (case opt ,@case-clauses))))))))
