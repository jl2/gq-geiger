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

(defun baud-type (n)
  (find n '(1200
            2400
            4800
            9600
            14400
            19200
            28800
            38400
            57600
            115200)))

(deftype geiger-baud-type ()
  '(satisfies baud-type))

(defun find-geiger (&key (baud 115200) (timeout-ms 5000))
   (make-instance 'geiger-counter
                  :path (car (uiop:directory-files "/dev/" "ttyUSB*"))
                  :baud baud
                  :timeout timeout-ms))


(defclass geiger-counter ()
  ((path
    :initform (car (uiop:directory-files "/dev/" "ttyUSB*"))
    :initarg :path
    :type (or string path)
    :documentation "The path to the GQ Geiger Counter serial device.")
   (baud
    :initform 115200
    :initarg :baud
    :type geiger-baud-type
    :documentation "Baud to use when communicating with the geiger counter on the serial port.")
   (timeout :initform 500
            :initarg :timeout
            :type fixnum
            :documentation "The timeout used to read and write geiger counter commands.")))

(defun geiger (&key (path (car (uiop:directory-files "/dev/" "ttyUSB*"))) (baud 115200) (timeout-ms 500))
  (make-instance 'geiger-counter
                 :path path
                 :baud baud
                 :timeout timeout-ms))

(declaim (ftype (function
                 ((simple-array (unsigned-byte 8)) fixnum)
                 (unsigned-byte 16))
                get-u2)
         (inline get-u2)

         (ftype (function
                 ((simple-array (unsigned-byte 8)) fixnum)
                 (unsigned-byte 32))
                get-u4)
         (inline get-u4)

         (ftype (function
                 ((simple-array (unsigned-byte 8)) fixnum)
                 (signed-byte 32))
                get-s4)
         (inline get-s4))

(defun get-u2 (arr idx)
  "Interpret two bytes in arr as an '(unsigned-byte 32)"
  (declare
   (optimize (speed 3) (space 0) (safety 0) (debug 0))
   (type (simple-array (unsigned-byte 8)) arr))
  (the (unsigned-byte 16) (+ (* (aref arr (1+ idx)) 256) (aref arr idx))))


(defun get-u4 (arr idx)
  "Interpret the four bytes in arr as an '(unsigned-byte 32)"
  (declare
   (optimize (speed 3) (space 0) (safety 0) (debug 0))
   (type (simple-array (unsigned-byte 8)) arr))
  (the (unsigned-byte 32) (+ (* (+ (* (+ (* (aref arr (+ 0 idx)) 256)
                                         (aref arr (+ 1 idx))) 256)
                                   (aref arr (+ 2 idx))) 256)
                             (aref arr (+ 3 idx)))))

(defun get-s4 (arr idx)
  "Interpret four bytes in arr as an '(signed-byte 32)"
  (declare
   (optimize (speed 3) (space 0) (safety 0) (debug 0))
   (type (simple-array (unsigned-byte 8)) arr))
  (the (signed-byte 32) (+ (* (+ (* (+ (* (aref arr (+ 0 idx)) 256)
                                       (aref arr (+ 1 idx))) 256)
                                 (aref arr (+ 2 idx))) 256)
                           (aref arr (+ 3 idx)))))


(defun build-command-string (command args)
  (format nil "<~a~{~^ ~x~}>>" (string-upcase command) args))

(defun send-command (geiger command args return-type)
  (with-slots (timeout path baud) geiger
    (cserial-port:with-serial (sp
                               path
                               :baud-rate baud
                               :data-bits 8
                               :parity :none
                               :stop-bits 1
                               :encoding :ascii)

      (cserial-port:with-timeout (timeout)
        (let ((cmd (build-command-string command args)))
          (cserial-port:write-serial-string cmd sp)

          (cond
            ((and (consp return-type)
                  (eq (car return-type) :string))
             (let ((string (make-string (cadr return-type))))
               (cserial-port:read-serial-string string sp)
               string))

            ((and (consp return-type)
                  (eq (car return-type) :array))
             (let ((buffer (make-array (cadr return-type) :element-type '(unsigned-byte 8))))
               (cserial-port:read-serial-byte-vector buffer sp)
               buffer))

            ((eq return-type :unsigned-int32)
             (let ((buffer (make-array 4 :element-type '(unsigned-byte 8))))
               (cserial-port:read-serial-byte-vector buffer sp)
               (get-u4 buffer 0)))

            ((eq return-type :signed-int32)
             (let ((buffer (make-array 4 :element-type '(unsigned-byte 8))))
               (cserial-port:read-serial-byte-vector buffer sp)
               (get-s4 buffer 0)))

            ((eq return-type :byte)
             (cserial-port:read-serial-byte sp))

            ((null return-type)
             nil)

            (t
             (error "Unknown return type ~a" return-type))))))))


(defun get-version (&optional (geiger (find-geiger)))
  (send-command geiger "GETVER" nil '(:string 15)))

(defun reboot (&optional (geiger (find-geiger)))
  (send-command geiger "REBOOT" nil nil))

(defun get-cpm (&optional (geiger (find-geiger)))
  (send-command geiger "GETCPM" nil :unsigned-int32))

(defun get-volt (&optional (geiger (find-geiger)))
  (send-command geiger "GETVOLT" nil '(:string 5)))

(defun get-config (&optional (geiger (find-geiger)))
  (send-command geiger "GETCFG" nil '(:array 512)))

(defun main (args)
  
  (format t "Hello!  This tool is REPL only for now.~%")
  (format t "Args: ~{~a~^ ~}~%" args)
  (let ((counter (geiger :path (car args))))
    (format t "Version: ~s~%" (get-version counter))
    (format t "Current CPM: ~d" (get-cpm counter)))
  0)
