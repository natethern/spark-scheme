(module contract-ds-helpers mzscheme
  (provide ensure-well-formed
           build-func-params
           build-clauses
           build-enforcer-clauses
           generate-arglists)
  
  (require (lib "list.ss")
           "contract-opt-guts.ss")
  (require-for-template mzscheme)

#|

With this definition:

(define-contract s (a b c))

this:

(s/dc [x e-x]
      [y () e-y]
      [z (x y) e-z])

expands into procedures & structs like this:

(let ([c-x e-x]
      [c-y (lambda (_) e-y)]
      [c-z (lambda (x y) e-z)])
  ... c-* variables get put into the contract struct ...

which are then called when the contract's fields are explored

|#

  (define (build-clauses name coerce-contract stx clauses)
    (let* ([field-names 
            (let loop ([clauses (syntax->list clauses)])
              (cond
                [(null? clauses) null]
                [else 
                 (let ([clause (car clauses)])
                   (syntax-case* clause (where and) raw-comparison?
                     [where null]
                     [and null]
                     [(id . whatever) (cons (syntax id) (loop (cdr clauses)))]
                     [else (raise-syntax-error name 
                                               "expected a field name and a contract together"
                                               stx
                                               clause)]))]))]
           [all-ac-ids (generate-temporaries field-names)]
           [defeat-inlining 
             ;; makes the procedure confusing enough so that
             ;; inlining doesn't consider it. this makes the 
             ;; call to procedure-closure-contents-eq? work 
             ;; properly
             (λ (e)
               (let loop ([n 20])
                 (if (zero? n)
                     e
                     #`(if (zero? (random 1))
                           #,(loop (- n 1))
                           (/ 1 0)))))])
      (let loop ([clauses (syntax->list clauses)]
                 [ac-ids all-ac-ids]
                 [prior-ac-ids '()]
                 [maker-args '()])
        (cond
          [(null? clauses)
           (with-syntax ([(maker-args ...) (reverse maker-args)])
             (syntax ((maker-args ... #f)
                      ())))]
          [else 
           (let ([clause (car clauses)])
             (syntax-case* clause (and where) raw-comparison?
               [where
                (build-clauses/where name stx (cdr clauses) field-names (reverse maker-args))]
               [and
                (build-clauses/and name stx (cdr clauses) '() '() (reverse maker-args))]
               [else
                (let ([ac-id (car ac-ids)])
                  (syntax-case clause ()
                    [(id (x ...) ctc-exp)
                     (and (identifier? (syntax id))
                          (andmap identifier? (syntax->list (syntax (x ...)))))
                     (let ([maker-arg #`(λ #,(match-up (reverse prior-ac-ids)
                                                       (syntax (x ...))
                                                       field-names)
                                          #,(defeat-inlining
                                              #`(#,coerce-contract '#,name ctc-exp)))])
                       (loop (cdr clauses)
                             (cdr ac-ids)
                             (cons (car ac-ids) prior-ac-ids)
                             (cons maker-arg maker-args)))]
                    [(id (x ...) ctc-exp)
                     (begin
                       (unless (identifier? (syntax id))
                         (raise-syntax-error name "expected identifier" stx (syntax id)))
                       (for-each (λ (x) (unless (identifier? x)
                                          (raise-syntax-error name "expected identifier" stx x)))
                                 (syntax->list (syntax (x ...)))))]
                    [(id ctc-exp)
                     (identifier? (syntax id))
                     (loop (cdr clauses)
                           (cdr ac-ids)
                           (cons (car ac-ids) prior-ac-ids)
                           (cons #`(#,coerce-contract '#,name ctc-exp) maker-args))]
                    [(id ctc-exp)
                     (raise-syntax-error name "expected identifier" stx (syntax id))]
                    [_
                     (raise-syntax-error name "expected name/identifier binding" stx clause)]))]))]))))
  
  (define (build-clauses/where name stx clauses field-names maker-args)
    (with-syntax ([(field-names ...) field-names])
      (let loop ([clauses clauses]
                 [vars '()]
                 [procs '()])
        (cond
          [(null? clauses) 
           ;; if there is no `and' clause, assume that it is always satisfied
           (build-clauses/and name stx (list (syntax #t)) vars procs maker-args)]
          [else 
           (let ([clause (car clauses)])
             (syntax-case* clause (and) raw-comparison?
               [and (build-clauses/and name stx (cdr clauses) vars procs maker-args)]
               [(id exp) 
                (identifier? (syntax id))
                (loop (cdr clauses)
                      (cons (syntax id) vars)
                      (cons (syntax (λ (field-names ...) exp)) procs))]
               [(id exp)
                (raise-syntax-error name "expected an identifier" stx (syntax id))]
               [_ 
                (raise-syntax-error name "expected an identifier and an expression" stx clause)]))]))))
  
      
  
  (define (build-enforcer-clauses opt/i opt/info name stx clauses f-x/vals f-xs/vals
                                  helper-id helper-info helper-freev)  
    (define (opt/enforcer-clause id stx)
      (syntax-case stx ()
        [(f arg ...)
         ;; we need to override the default optimization of recursive calls to use our helper
         (and (opt/info-recf opt/info) (module-identifier=? (opt/info-recf opt/info) #'f))
         (values
          #`(f #,id arg ...)
          null
          null
          null
          #f
          #f
          null)]
        [else (opt/i (opt/info-change-val id opt/info)
                     stx)]))

    (let* ([field-names 
            (map (λ (clause)
                   (syntax-case clause ()
                     [(id . whatever) (syntax id)]
                     [else (raise-syntax-error name 
                                               "expected a field name and a contract together"
                                               stx
                                               clause)]))
                 (syntax->list clauses))]
           [all-ac-ids (generate-temporaries field-names)])
      (let loop ([clauses (syntax->list clauses)]
                 [let-vars f-x/vals]
                 [arglists f-xs/vals]
                 [ac-ids all-ac-ids]
                 [prior-ac-ids '()]
                 [maker-args '()]
                 [lifts-ps '()]
                 [superlifts-ps '()]
                 [stronger-ribs-ps '()])
        (cond
          [(null? clauses)
           (values (reverse maker-args)
                   lifts-ps
                   superlifts-ps
                   stronger-ribs-ps)]
          [else
           (let ([clause (car clauses)]
                 [let-var (car let-vars)]
                 [arglist (car arglists)]
                 [ac-id (car ac-ids)])
             (syntax-case clause ()
               [(id (x ...) ctc-exp)
                (and (identifier? (syntax id))
                     (andmap identifier? (syntax->list (syntax (x ...)))))
                (let*-values ([(next lifts superlifts partials _ _2 _3)
                               (opt/enforcer-clause let-var (syntax ctc-exp))]
                              [(maker-arg)
                               (with-syntax ([val (opt/info-val opt/info)]
                                             [(new-let-bindings ...) 
                                              (match-up/bind (reverse prior-ac-ids)
                                                             (syntax (x ...))
                                                             field-names
                                                             arglist)])
                                 #`(#,let-var
                                      #,(bind-lifts
                                         superlifts
                                         #`(let (new-let-bindings ...)
                                             #,(bind-lifts 
                                                (append lifts partials)
                                                next)))))])
                  (loop (cdr clauses)
                        (cdr let-vars)
                        (cdr arglists)
                        (cdr ac-ids)
                        (cons (car ac-ids) prior-ac-ids)
                        (cons maker-arg maker-args)
                        lifts-ps
                        superlifts-ps
                        stronger-ribs-ps))]
               [(id (x ...) ctc-exp)
                (begin
                  (unless (identifier? (syntax id))
                    (raise-syntax-error name "expected identifier" stx (syntax id)))
                  (for-each (λ (x) (unless (identifier? x)
                                     (raise-syntax-error name "expected identifier" stx x)))
                            (syntax->list (syntax (x ...)))))]
               [(id ctc-exp)
                (identifier? (syntax id))
                (let*-values ([(next lifts superlifts partials _ __ stronger-ribs)
                               (opt/enforcer-clause let-var (syntax ctc-exp))]
                              [(maker-arg)
                               (with-syntax ((val (opt/info-val opt/info)))
                                 #`(#,let-var
                                      #,(bind-lifts
                                         partials
                                         next)))])
                  (loop (cdr clauses)
                        (cdr let-vars)
                        (cdr arglists)
                        (cdr ac-ids)
                        (cons (car ac-ids) prior-ac-ids)
                        (cons maker-arg maker-args)
                        (append lifts-ps lifts)
                        (append superlifts-ps superlifts)
                        (append stronger-ribs-ps stronger-ribs)))]
               [(id ctc-exp)
                (raise-syntax-error name "expected identifier" stx (syntax id))]))]))))
  
  (define (build-clauses/and name stx clauses synth-names synth-procs maker-args)
    (unless (pair? clauses)
      (raise-syntax-error name "expected an expression after `and' keyword" stx))
    (unless (null? (cdr clauses))
      (raise-syntax-error name "expected only one expression after `and' keyword" stx (cadr clauses)))
    (with-syntax ([(maker-args ...) maker-args]
                  [(synth-names ...) synth-names]
                  [(synth-procs ...) synth-procs]
                  [exp (car clauses)])
      (syntax ((maker-args ... (list (λ (ht) (let ([synth-names (hash-table-get ht 'synth-names)] ...) exp))
                                     (cons 'synth-names synth-procs) ...))
               (synth-names ...)))))
  
  (define (raw-comparison? x y) 
    (and (identifier? x)
         (identifier? y)
         (eq? (syntax-e x) (syntax-e y))))
  
  ;; generate-arglists : (listof X) -> (listof (listof X))
  ;; produces the list of arguments to the dependent contract
  ;; functions, given the names of some variables.
  ;; eg: (generate-arglists '(x y z w))
  ;;  =  (list '() '(x) '(x y) '(x y z))
  (define (generate-arglists vars)
    (reverse
     (let loop ([vars (reverse vars)])
       (cond
         [(null? vars) null]
         [else (cons (reverse (cdr vars)) 
                     (loop (cdr vars)))]))))
  
  (define (match-up/bind prior-ac-ids used-field-names field-names rhss)
    (let ([used-field-ids (syntax->list used-field-names)])
      (let loop ([prior-ac-ids prior-ac-ids]
                 [field-names field-names]
                 [rhss rhss])
        (cond
          [(null? prior-ac-ids) null]
          [else (let* ([ac-id (car prior-ac-ids)]
                       [field-name (car field-names)]
                       [id-used 
                        (ormap (λ (used-field-id) 
                                 (and (eq? (syntax-e field-name) (syntax-e used-field-id))
                                      used-field-id))
                               used-field-ids)])
                  (if id-used
                      (cons (with-syntax ([id id-used]
                                          [arg (car rhss)])
                              #'[id arg])
                            (loop (cdr prior-ac-ids)
                                  (cdr field-names)
                                  (cdr rhss)))
                      (loop (cdr prior-ac-ids)
                            (cdr field-names)
                            (cdr rhss))))]))))
  
  (define (match-up prior-ac-ids used-field-names field-names)
    (let ([used-field-ids (syntax->list used-field-names)])
      (let loop ([prior-ac-ids prior-ac-ids]
                 [field-names field-names])
        (cond
          [(null? prior-ac-ids) null]
          [else (let* ([ac-id (car prior-ac-ids)]
                       [field-name (car field-names)]
                       [id-used 
                        (ormap (λ (used-field-id) 
                                 (and (eq? (syntax-e field-name) (syntax-e used-field-id))
                                      used-field-id))
                               used-field-ids)])
                  (if id-used
                      (cons id-used
                            (loop (cdr prior-ac-ids)
                                  (cdr field-names)))
                      (cons (car (generate-temporaries '(ignored-arg)))
                            (loop (cdr prior-ac-ids)
                                  (cdr field-names)))))]))))
  
  (define (sort-wrt name stx ids current-order-field-names desired-order-field-names)
    (let ([id/user-specs (map cons ids current-order-field-names)]
          [ht (make-hash-table)])
      (let loop ([i 0]
                 [orig-field-names desired-order-field-names])
        (unless (null? orig-field-names) 
          (hash-table-put! ht (syntax-e (car orig-field-names)) i)
          (loop (+ i 1) (cdr orig-field-names))))
      (let* ([lookup
              (λ (id-pr)
                (let ([id (car id-pr)]
                      [use-field-name (cdr id-pr)])
                  (hash-table-get ht
                                  (syntax-e use-field-name) 
                                  (λ () 
                                    (raise-syntax-error name "unknown field name" stx use-field-name)))))]
             [cmp (λ (x y) (<= (lookup x) (lookup y)))]
             [sorted-id/user-specs (sort id/user-specs cmp)])
        (map car sorted-id/user-specs))))
    

  (define (find-matching all-ac-ids chosen-ids field-names)
    (map (λ (chosen-id)
           (let* ([chosen-sym (syntax-e chosen-id)]
                  [id (ormap (λ (ac-id field-name)
                               (and (eq? (syntax-e field-name) chosen-sym)
                                    ac-id))
                             all-ac-ids
                             field-names)])
             (unless id
               (error 'find-matching "could not find matching for ~s" chosen-id))
             id))
         (syntax->list chosen-ids)))

  
  (define (build-func-params ids)
    (let ([temps (generate-temporaries ids)])
      (let loop ([ids (syntax->list ids)]
                 [temps temps]
                 [can-refer-to '()])
        (cond
          [(null? ids) null]
          [else (cons
                 (append (reverse can-refer-to) temps)
                 (loop (cdr ids)
                       (cdr temps)
                       (cons (car ids) can-refer-to)))]))))
  
  (define (ensure-well-formed stx field-count)
    (syntax-case stx ()
      [(_ [id exp] ...)
       (and (andmap identifier? (syntax->list (syntax (id ...))))
            (equal? (length (syntax->list (syntax (id ...))))
                    field-count))
       (void)]
      [(_ [id exp] ...)
       (andmap identifier? (syntax->list (syntax (id ...))))
       (raise-syntax-error 'struct/dc 
                           (format "expected ~a clauses, but found ~a"
                                   field-count
                                   (length (syntax->list (syntax (id ...)))))
                           stx)]
      [(_ [id exp] ...)
       (for-each
        (λ (id) (unless (identifier? id) (raise-syntax-error 'struct/dc "expected identifier" stx id)))
        (syntax->list (syntax (id ...))))])))

  
