(module simple-logger

(
 ;; Configuration parameters
 log-level
 info-logger-config
 warning-logger-config
 error-logger-config
 debug-logger-config

 ;; logger-config record
 make-logger-config
 logger-config?
 logger-config-prefix
 logger-config-port
 logger-config-level
 logger-config-prefix-set!
 logger-config-port-set!
 logger-config-level-set!

 ;; Logger procedures
 log-info
 log-warning
 log-error
 log-debug
 die!

 ;; Utils
 make-logger
 config-logger
)

(import scheme)
(import (chicken base)
        (chicken format))

(define-record logger-config prefix port level)

(define %make-logger-config make-logger-config)

(define (make-logger-config #!key (prefix "") (port (current-error-port)) (level 0))
  (%make-logger-config prefix port level))

(define log-level
  (make-parameter 30))

(define debug-logger-config
  (make-parameter
   (make-logger-config prefix: (lambda () "[DEBUG] ") level: 10)))

(define info-logger-config
  (make-parameter
   (make-logger-config prefix: (lambda () "[INFO] ") level: 20)))

(define warning-logger-config
  (make-parameter
   (make-logger-config prefix: (lambda () "[WARNING] ") level: 30)))

(define error-logger-config
  (make-parameter
   (make-logger-config prefix: (lambda () "[ERROR] ") level: 40)))

(define (config-logger logger-config #!key prefix port level)
  (make-logger-config prefix: (or prefix (logger-config-prefix logger-config))
                      port: (or port (logger-config-port logger-config))
                      level: (or level (logger-config-level logger-config))))

(define (printer prefix port/path fmt args)
  (let ((print-to-port
         (lambda (port)
           (apply fprintf
                  (cons port (cons (string-append (prefix)  fmt "\n") args)))
           (flush-output port))))
    (if (port? port/path)
        (print-to-port port/path)
        (call-with-output-file port/path print-to-port append:))))

(define (make-logger config #!optional thunk)
  (lambda (fmt . args)
    (when (>= (logger-config-level (config)) (log-level))
      (printer (logger-config-prefix (config))
               (logger-config-port (config))
               fmt
               args))
    (when thunk
      (thunk))))

(define log-info (make-logger info-logger-config))
(define log-warning (make-logger warning-logger-config))
(define log-error (make-logger error-logger-config))
(define log-debug (make-logger debug-logger-config))

(define die! (make-logger error-logger-config (lambda () (exit 1))))

) ;; end module
