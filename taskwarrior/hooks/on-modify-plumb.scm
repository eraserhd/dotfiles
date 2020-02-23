(include "json#.scm")

(define-macro (if-let binding then . else)
  (let ((tmp (gensym)))
    `(let ((,tmp ,(cadr binding)))
       (if ,tmp
         (let ((,(car binding) ,tmp))
           ,then)
         ,@else))))

(define (started? task)
  (not (eq? #f (table-ref task "start" #f))))

(define (plumb-annotations task)
  (for-each
    (lambda (annotation)
      (if-let [desc (table-ref annotation "description" #f)]
        (let ((proc (open-process (list path: "9" arguments: (list "plumb" desc)))))
          (process-status proc)
          (close-port proc))))
    (vector->list (table-ref task "annotations" #()))))

(define (main . args)
  (let ((original-task (json-read (current-input-port)))
        (modified-task (json-read (current-input-port))))
    (json-write modified-task (current-output-port))
    (newline)
    (if (and (not (started? original-task))
             (started? modified-task))
      (plumb-annotations modified-task))
    0))

;(exit (apply main (command-line)))
