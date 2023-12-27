(import (chicken io)
        (chicken file)
        (chicken format))
(import test)
(import simple-logger)

(define (call-with-tmp-file proc)
  (let ((tmp (create-temporary-file)))
    (call-with-output-file tmp proc)
    (let ((log-message (with-input-from-file tmp read-string)))
      (delete-file tmp)
      log-message)))

(test-begin)

(test-begin "using default log-level")

(test "debug"
      #!eof
      (call-with-tmp-file
       (lambda (port)
         (debug-logger-config (config-logger (debug-logger-config) port: port))
         (log-debug "foo"))))

(test "info"
      #!eof
      (call-with-tmp-file
       (lambda (port)
         (info-logger-config (config-logger (info-logger-config) port: port))
         (log-info "foo"))))

(test "warning"
      "[WARNING] foo\n"
      (call-with-tmp-file
       (lambda (port)
         (warning-logger-config (config-logger (warning-logger-config) port: port))
         (log-warning "foo"))))

(test "error"
      "[ERROR] foo\n"
      (call-with-tmp-file
       (lambda (port)
         (error-logger-config (config-logger (error-logger-config) port: port))
         (log-error "foo"))))

(test-end "using default log-level")

(test-group
 "changing prefix"
 (test "[FOO] bar"
       "[FOO] bar\n"
       (call-with-tmp-file
        (lambda (port)
          (error-logger-config (config-logger (error-logger-config)
                                              port: port
                                              prefix: (lambda () "[FOO] ")))
          (log-error "bar")))))

(test-begin "changing log level")

 (test "No logging when log-level is 100"
       #!eof
       (parameterize ((log-level 100))
         (call-with-tmp-file
          (lambda (port)
            (error-logger-config
             (config-logger (error-logger-config) port: port))
            (log-error "foo")))))

 (test "Logging debug when log-level is 0"
       "[DEBUG] foo\n"
       (parameterize ((log-level 0))
         (call-with-tmp-file
          (lambda (port)
            (debug-logger-config
             (config-logger (debug-logger-config) port: port))
            (log-debug "foo")))))

(test-end "changing log level")


(test-group
 "creating a custom logger"
 (let* ((custom-logger-config
         (make-parameter
          (make-logger-config prefix: (lambda () "[CUSTOM] ") level: 50)))
        (log-custom (make-logger custom-logger-config)))
   (test "custom logger"
         "[CUSTOM] foo\n[CUSTOM] bar\n"
         (call-with-tmp-file
          (lambda (port)
            (custom-logger-config
             (config-logger (custom-logger-config) port: port))
            (log-custom "foo")
            (log-custom "bar"))))))

(test-group
 "using a file for a custom logger"
 (let* ((tmp (create-temporary-file))
        (custom-logger-config
         (make-parameter
          (make-logger-config prefix: (lambda () "[CUSTOM] ") port: tmp level: 50)))
        (log-custom (make-logger custom-logger-config)))
   (log-custom "foo")
   (log-custom "bar")
   (let ((log (with-input-from-file tmp read-string)))
     (delete-file tmp)
     (test "custom logger" "[CUSTOM] foo\n[CUSTOM] bar\n" log))))

(test-group
 "using a debug logger with a dynamic level"
 (let* ((custom-debug-logger-config
         (make-parameter (make-logger-config)))
        (debug
         (let ((logger (make-logger custom-debug-logger-config)))
           (lambda (level fmt . args)
             (parameterize ((custom-debug-logger-config
                             (config-logger
                              (custom-debug-logger-config)
                              prefix: (lambda () (sprintf "[DEBUG ~a] " level))
                              level: (* level 10))))
               (apply logger (cons fmt args)))))))
   (test "debug level 1"
         #!eof
         (call-with-tmp-file
          (lambda (port)
            (custom-debug-logger-config
             (config-logger (custom-debug-logger-config) port: port))
            (debug 1 "Debug level 1 (won't be printed)"))))

   (test "debug level 5"
         "[DEBUG 5] Debug level 5\n"
         (call-with-tmp-file
          (lambda (port)
            (custom-debug-logger-config
             (config-logger (custom-debug-logger-config) port: port))
            (debug 5 "Debug level 5"))))))

(test-end)
