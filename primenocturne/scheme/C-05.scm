(load "./lib")
"P.536"
"C.5 データ構造と探索"
"C.5.1 データ構造"
(define stack '())
(define set-stack
  (lambda ()
    (set! stack '())
    'done))
(define push
  (lambda (x)
    (cond ((null? stack) (set! stack (list x)))
          (else (set! stack (cons x stack))))
    stack))
"P.537"
(push 'a)
(push 'b)
(push 'c)

(define pop
  (lambda ()
    (let ((tmp '()))
      (cond ((null? stack) '())
            (else (set! tmp   (car stack))
                  (set! stack (cdr stack))
                  tmp)))))
(pop)
(pop)
(pop)
(pop)

(define queue '())
"P.538"
(define tail '())
(define set-queue
  (lambda ()
    (set! queue '())
    (set! tail  '())
    'done))
(define enqueue
  (lambda (x)
    (cond ((null? queue)
           (set!          queue (list x))
           (set!          tail  queue))
          (else (set-cdr! tail  (list x))
                (set!     tail  (cdr tail))))
    queue))
(define dequeue
  (lambda ()
    (let ((tmp '()))
      (cond ((null? queue) '())
            (else (set! tmp   (car queue))
                  (set! queue (cdr queue)) tmp)))))
"P.539"
(enqueue 'a)
(enqueue 'b)
(enqueue 'c)
(dequeue)
(dequeue)
(dequeue)
(dequeue)

"C.5.2 探索の手法"
(define data '((a1 (b1) (b2)) (a2 (b3))))
"P.540"
"深さ優先探索"
(car '(a b))
(cdr '(a b))
(cadr '(a b))

"P.544"
(define search
  (lambda (name tree)
    (define path
      (lambda (insert delete dig)
        (let loop ((tmp '()))
          (let ((w (delete)))
            (if (null? w)
                (reverse tmp)
                (let ((head (car w)) (tail (cdr w)))
                  (cond ((pair? head)
                         (cond ((pair? tail) (dig w)
                                (loop tmp))
                               (else (insert head)
                                     (loop tmp))))
                        (else
                         (cond ((pair? tail) (insert tail)
                                (loop (cons head tmp)))
                               (else
                                (loop (cons head tmp))))))))))))
    (cond ((string=? name "dfs") (push tree)
           (path push pop dig-dfs))
          (else (enqueue tree)
                (path enqueue dequeue dig-bfs)))))
(define dig-dfs
  (lambda (x)
    (cond ((null? (cdr x)) (push (car x)))
          (else (dig-dfs (cdr x)) (push (car x))))))
"P.545"
(define test '((a) (B)))
(dig-dfs test)
(search "dfs" data)

"P.546"
"幅優先探索"
(define dig-bfs
  (lambda (x)
    (cond ((null? (cdr x)) (enqueue (car x)))
          (else (enqueue (car x)) (dig-bfs (cdr x))))))
(search "bfs" data)

"P.548"
(define dfs-w
  (lambda (tree)
    (define chk
      (lambda (tree)
        (cond ((null?    tree) (reverse stack))
              ((nonpair? tree) (push tree))
              (else (chk (car tree))
                    (chk (cdr tree))))))
    (set-stack) (chk tree)))
(dfs-w data)
