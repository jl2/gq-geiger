;; gq-geiger.lisp
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
  (the (unsigned-byte 16) (+ (ash (aref arr idx) 8)
                             (aref arr (1+ idx)))))


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
  "Create a command string to send to the GQ geiger counter serial device.
See gq-documentation/GQ-RFC1801.txt for details.  In short, this converts
 command args -> <command arg1 arg2 arg3 ...>>"
  (format nil "<~a~{~^ ~x~}>>" (string-upcase command) args))

(defun send-command (geiger command &optional return-type &rest args)
  "Send a command with arguments to the geiger counter and read a value of the given type in return.
Type can be:
  '(:string <n>)    - an ascii string of size <n>
  '(:array <n>)     - a byte array of size <n>
  :unsigned-int32   - an unsigned 32-bit integer as a lisp integer type
  :signed-int32     - a signed 32-bit integer as a lisp integer type
  :byte             - an 8-bit integer as a lisp integer type
  nil               - Indicates no return value

Where nil means 'no value'.  '(:string n) an '(:array n) can have any size up to 4096."
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
            ;; '(:string <size>)
            ((and (consp return-type)
                  (eq (car return-type) :string))
             (let ((string (make-string (cadr return-type))))
               (cserial-port:read-serial-string string sp)
               string))

            ;; '(:array <size>)
            ((and (consp return-type)
                  (eq (car return-type) :array))
             (let ((buffer (make-array (cadr return-type)
                                       :element-type '(unsigned-byte 8))))
               (cserial-port:read-serial-byte-vector buffer sp)
               buffer))

            ((eq return-type :unsigned-int32)
             (let ((buffer (make-array 4
                                       :element-type '(unsigned-byte 8))))
               (cserial-port:read-serial-byte-vector buffer sp)
               (get-u4 buffer 0)))

            ((eq return-type :signed-int32)
             (let ((buffer (make-array 4
                                       :element-type '(unsigned-byte 8))))
               (cserial-port:read-serial-byte-vector buffer sp)
               (get-s4 buffer 0)))

            ((eq return-type :byte)
             (cserial-port:read-serial-byte sp))

            ((null return-type)
             nil)

            (t
             (error "Unknown return type ~a" return-type))))))))


(defun get-version (&optional (geiger (find-geiger)))
  "Return information about the geiger counter model and firmware revision."
  (send-command geiger
                "GETVER"
                '(:string 15)))

(defun reboot (&optional (geiger (find-geiger)))
  "Reboot the geiger counter."
  (send-command geiger
                "REBOOT"
                nil))

(defun power-off (&optional (geiger (find-geiger)))
  "Power off the geiger counter."
  (send-command geiger
                "POWEROFF"
                nil))

(defun get-cpm (&optional (geiger (find-geiger)))
  "Return the current counts per minute (CPM) reading."
  (send-command geiger
                "GETCPM"
                :unsigned-int32))

(defun get-volt (&optional (geiger (find-geiger)))
  "Get the battery voltage status."
  (send-command geiger
                "GETVOLT"
                '(:string 5)))

(defun get-config (&optional (geiger (find-geiger)))
  "Return a 512 byte configuration data buffer."
  (send-command geiger
                "GETCFG"
                '(:array 512)))

(defun get-gyro (&optional (geiger (find-geiger)))
  "Return a 512 byte configuration data buffer."
  (let* ((raw-value (send-command geiger
                                 "GETGYRO"
                                 '(:array 7)))
         (x (get-u2 raw-value 0))
         (y (get-u2 raw-value 2))
         (z (get-u2 raw-value 4)))
    (values  (list x y z)
             raw-value)))

(defun get-date-time (&optional (geiger (find-geiger)))
  "Return the current year, month, day, hour, minute, and second values."
  (let* ((raw-values (send-command geiger
                                  "GETDATETIME"
                                  '(:array 7)))
        (universal (encode-universal-time (aref raw-values 5)
                                          (aref raw-values 4)
                                          (aref raw-values 3)
                                          (aref raw-values 2)
                                          (aref raw-values 1)
                                          (aref raw-values 0))))
    (values (local-time:universal-to-timestamp universal)
            universal
            raw-values)))

(defun set-timestamp (timestamp &key
                                  (geiger (find-geiger)))
  "Set the date and time using a local-time:timestamp."
  (multiple-value-bind
        (second minute hour day month year) (cl:decode-universal-time (local-time:timestamp-to-universal timestamp))
    (set-date-time :atomic t
                   :year year
                   :month month
                   :day day
                   :hour hour
                   :minute minute
                   :second second
                   :geiger geiger)))

(defun set-date-time (&key
                        (year :now)
                        (month :now)
                        (day :now)
                        (hour :now)
                        (minute :now)
                        (second :now)
                        (atomic t)
                        (geiger (find-geiger)))
  "Set the date and time on the counter.  When atomic is non-nil the time is set with one SETDATETIME command.
When atomic=nil individual SETDATE** and SETTIME** commands are used."

  (multiple-value-bind
        (second-now minute-now hour-now day-now month-now year-now) (cl:decode-universal-time (cl:get-universal-time))
    (cond (atomic
           (send-command geiger
                         "SETDATETIME"
                         :byte
                         (format nil
                                 "~2,'0x~2,'0x~2,'0x~2,'0x~2,'0x~2,'0x"
                                 (mod (if (eq year :now)
                                          year-now year)
                                      2000)
                                 (if (eq  month :now)  month-now month)
                                 (if (eq  day :now)  day-now day)
                                 (if (eq  hour :now)  hour-now hour)
                                 (if (eq  minute :now)  minute-now minute)
                                 (if (eq  second :now)  second-now second))))

          (t
           (loop :for (cmd . param) :in `(("DATEYY" . ,(if (eq year :now) year-now year))
                                          ("DATEMM" . ,(if (eq  month :now)  month-now month))
                                          ("DATEDD" . ,(if (eq  day :now)  day-now day))
                                          ("TIMEHH" . ,(if (eq  hour :now)  hour-now hour))
                                          ("TIMEMM" . ,(if (eq  minute :now)  minute-now minute))
                                          ("TIMESS" . ,(if (eq  second :now)  second-now second)))
                 :do
                    (send-command geiger
                                  (format nil "SET~a" cmd)
                                  :byte param))))))
