# gq-geiger

### _Jeremiah LaRocco <jeremiah_larocco@fastmail.com>_
A package to control GQ Geiger counters.

### Example
```common-lisp
 (ql:quickload :gq-geiger)
 (format t "Version: ~a~%CPM: ~a~%" (gq:get-version) (gq:get-cpm))
 ;; Version: GMC-600+Re 2.52
 ;; CPM: 78
```
## License

ISC

Copyright (c) 2024 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


