#lang racket
(require txexpr pollen/core pollen/decode pollen/unstable/typography txexpr)

(module setup racket/base
  (provide (all-defined-out)))

(provide emph)
(define (emph . elements)
  (txexpr 'strong empty elements))

(provide newthought)
(define (newthought . elements)
  (txexpr 'span '((class "newthought")) elements))

(provide chinese)
(define (chinese . elements)
  (txexpr '@ empty elements))

(provide link)
(define (link url . xs)
  (@ `(a ((href ,url)) ,@xs)

  )
)

(provide heading subtitle subsubtitle)
(define (heading . elements)
  (txexpr 'h1 '((class "sans upper")) elements))
(define (subtitle . elements)
  (txexpr 'h2 '((class "sans upper")) elements))
(define (subsubtitle . elements)
  (txexpr 'h3 '((class "sans upper")) elements))

(provide image)
(define (image name)
  `(img ((src ,(string-append "images/" name))(alt ,name)))
)

(provide sidenote marginnote)
(define (sidenote label . elements)
  (@ `(label ((for ,label)(class "margin-toggle sidenote-number")))
     `(input ((id ,label)(class "margin-toggle")(type "checkbox")))
     (txexpr 'span '((class "sidenote")) elements)
  )
)

(define (marginnote label toggle . elements)
  (@ `(label ((for ,label)(class "margin-toggle")) ,toggle)
     `(input ((id ,label)(class "margin-toggle")(type "checkbox")))
     (txexpr 'span '((class "marginnote")) elements)
  )
)

(provide makelist)
(define (makelist . xs)
  (define (break-paragraphs x)
    (cond [(string? x) (foldr (lambda (v l) (list* 'listitem v l)) '() (string-split x "\n"))]
          [else (list x)]))
  (define (split-by x lst)
    (foldr (λ (element next)
             (if (eqv? element x)
                 (cons empty next)
                 (cons (cons element (first next)) (rest next))))
           (list empty) lst))
  (let* ([xs (append-map break-paragraphs xs)]
        [xs (split-by 'listitem (cdr xs))]
        [xs (map (λ (x) (txexpr 'li empty x)) xs)])
        (txexpr 'ul empty xs)
  )
)

(define (insert-section-tags #:list-separator [list-separator "∎"] . elems)
  (define (split-by x lst)
    (foldr (lambda (element next)
             (if (eqv? element x)
                 (cons empty next)
                 (cons (cons element (first next)) (rest next))))
           (list empty) lst))
  (define sections (split-by list-separator elems))
  (map
      (λ (xs) (txexpr 'section empty xs))
      sections))

;root -- allow newlines instead of p tags
(provide root)
(define (root . elements)
   (decode-elements (apply insert-section-tags elements)
     #:txexpr-elements-proc decode-paragraphs
     #:exclude-tags '(pre)
     #:string-proc (compose1 smart-quotes smart-dashes))
   )
