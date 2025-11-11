(use-modules (ice-9 ftw)
             (ice-9 regex)
             (ice-9 rdelim)
             (ice-9 popen)
             (ice-9 string-fun)
             (ice-9 textual-ports))

(define (relative-path s base-dir)
  (let ((m (string-match 
             (format #f "~a/(([0-9A-Za-z-]+/)*[0-9A-Za-z-]+\\.[0-9A-Za-z-]+)" base-dir) 
             s)))
    (if (eq? m #f)
      (error (format (current-error-port) "invalid filename: ~a~%" s))
      (let ((m1 (match:substring m 1)))
        (if (eq? m1 #f)
          (error "this shouldn't happen, ever")
          m1)))))

(define (template-var s)
  (let ((m (string-match ".*\\{\\{ ([0-9A-Za-z-]+) \\}\\}.*" s)))
    (if (eq? m #f)
      #f
      (let ((count (match:count m)))
        (if (> count 2)
          (error (format 
                   (current-error-port) 
                   "more than one template variable per line isn't implemented: ~a~%" 
                   count))
          (let ((m1 (match:substring m 1)))
            (if (eq? m1 #f)
              (error "this shouldn't happen, ever")
              m1)))))))

(define git-rev
  (let* ((port (open-input-pipe "git rev-parse --short HEAD"))
         (rev (read-line port)))
    (close-pipe port)
    rev))

(define template-vars
  (list (cons "git-rev" git-rev)))

(define (build)
  (let ((stat (stat "out" #f)))
    (if (eq? stat #f)
      (mkdir "out")
    (when (not (eq? 'directory (stat:type stat)))
      (error "out is not a directory"))))

  (define (no-op name stat result) result)
  (define (leaf name stat result)
    (call-with-input-file name (lambda (in-port)
                                (let loop ((acc '()))
                                  (let ((line (get-line in-port)))
                                    (cond 
                                      ((eof-object? line)
                                        (call-with-output-file 
                                          (format #f "out/~a" 
                                                  (relative-path name "layouts")) 
                                          (lambda (out-port)
                                            (for-each (lambda (acc-line)
                                                        (display acc-line out-port)
                                                        (newline out-port))
                                                      (reverse acc)))))
                                      ((not (eq? (template-var line) #f))
                                       (let* ((var-name (template-var line))
                                              (var-val (assoc-ref template-vars var-name)))
                                         (if (eq? var-val #f)
                                           (error (format (current-error-port) 
                                                          "~a isn't a defined template variable~%"
                                                          var-name))
                                           (loop (cons 
                                                   (string-replace-substring 
                                                     line
                                                     (format #f "{{ ~a }}" var-name)
                                                     var-val)
                                                    acc)))))
                                      (else
                                        (loop (cons line acc))))))))
    (+ result (stat:size stat)))

  (define (%error name stat errno result)
    (format (current-error-port) "~a: ~a~%" name (strerror errno))
    result)
  (file-system-fold no-op leaf no-op no-op no-op error 0 "layouts"))