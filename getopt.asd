;;; getopt.asd - ASDF system definition for getopt
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

(asdf:defsystem "getopt"
                :description "POSIX-style getopt for Common Lisp"
                :author "imtomt"
                :license "GPL-3.0-only"
                :components ((:file "getopt")))
