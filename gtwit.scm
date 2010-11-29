
(use net.twitter)
(use sxml.sxpath)
(debug-print-width #f)

(define *cred* (make <twitter-cred>
                 :consumer-key         ""
                 :consumer-secret      ""
                 :access-token         ""
                 :access-token-secret  ""))

(define *since_id* 0)

(define (print-tweet statuses)
  (if (null? statuses)
      #f
      (let ((tweet (extract-tweet (cdar statuses) ())))
        (format #t "~a: ~a\n"
                ;(x->string *since_id*)
                (x->string (car tweet))
                (x->string (cadr tweet)))
        (print-tweet (cdr statuses)))))

(define (extract-tweet status res)
  (cond ((null? status) #f)
        ((and (string=? "id" (x->string (caar status)))
               (>= (x->number (cadar status)) *since_id*))
         (set! *since_id* (string->number (cadar status)))
         (extract-tweet (cdr status) res))
        ((string=? "text" (x->string (caar status)))
         (extract-tweet (cdr status) (cons (cadar status) res)))
        ((string=? "user" (x->string (caar status)))
         (extract-tweet (cdar status) res))
        ((string=? "screen_name" (x->string (caar status)))
         (cons (cadar status) res))
        (else (extract-tweet (cdr status) res))))

(define (main args)
  (let* ((timeline 
          (twitter-home-timeline/sxml *cred* :count 30))
         (statuses ((sxpath '(statuses status)) timeline)))
    (print-tweet (reverse statuses)))
  (while #t
    (let* ((timeline 
            (twitter-home-timeline/sxml *cred* :count 6 :since-id *since_id*))
           (statuses ((sxpath '(statuses status)) timeline)))
      (print-tweet (reverse statuses))
    (sys-sleep 100))))
