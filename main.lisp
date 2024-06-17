;; main.lisp
;; Copyright (c) 2024 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :gq-geiger)

(defun main (args)
  (cond
    ((find "--help" args)
     (format t "Print GQ geiger counter version and current CPM count to stdout.~%
First command line parameter is the path to the device in /dev/, probably /dev/ttyUSB*.~%
If no parameter is given, the first /dev/ttyUSB device is used. ~%~%"))
    (t
     (let ((counter (if args
                        (gq:geiger :path (car args))
                        (gq:find-geiger))))
       (format t "Version: ~s~%" (gq:get-version counter))
       (format t "Current CPM: ~d" (gq:get-cpm counter)))))
  0)
