(include "json#.scm")

(define-macro (if-let binding then . else)
  (let ((tmp (gensym)))
    `(let ((,tmp ,(cadr binding)))
       (if ,tmp
         (let ((,(car binding) ,tmp))
           ,then)
         ,@else))))

(define-macro (when-let binding . body)
  `(if-let ,binding ,@body))

(define-macro (doto x . exprs)
  (let* ((tmp (gensym))
         (doto-expr (lambda (expr)
                     (cond
                      ((symbol? expr) `(,expr ,tmp))
                      ((list? expr)   `(,(car expr) ,tmp ,@(cdr expr)))))))
    `(let ((,tmp ,x))
       ,@(map doto-expr exprs)
       ,tmp)))

(define (started? task)
  (not (eq? #f (table-ref task "start" #f))))

(define (plumb-annotations task)
  (for-each
    (lambda (annotation)
      (when-let [desc (table-ref annotation "description" #f)]
        (doto (open-process (list path: "9" arguments: (list "plumb" desc)))
          (process-status)
          (close-port))))
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

(exit (apply main (command-line)))
