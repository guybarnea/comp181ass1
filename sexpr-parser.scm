(load "pc.scm")


;(define <sexpr>
;    (disj <Boolean> <Char> <String> <Number> <Symbol>
;     <ProperList> <ImproperList> <Vector> <Quoted> <QuasiQuoted> <Unquoted> 
;	  <UnquoteAndSpliced> <CBName>  <InfixExtension> 
;))
(define list->number
  (lambda (base)
    (lambda (lst)
      (string->number (list->string lst) base))
  ))

(define list->hex-number (list->number 16))
(define list->base10-number (list->number 10))

(define <Boolean> 
  (new
   (*parser (word-ci "#t"))
   (*pack (lambda (_) #t))

   (*parser (word-ci "#f"))
   (*pack (lambda (_) #f))
   (*disj 2)
done))

(define <HexChar>
  (new
   (*parser (range #\0 #\9))
   (*parser (range-ci #\a #\f))
   (*disj 2)
  done))


(define <Natural>
  (new
   (*parser (range #\0 #\9))
   *plus
   (*pack list->base10-number)
done))

(define <Integer>
  (new
   (*parser (char #\+))
   (*parser <Natural>)
   (*caten 2)
   (*pack-with (lambda (sign num) num))

   (*parser (char #\-))
   (*parser <Natural>)
   (*caten 2)
   (*pack-with (lambda (sign num) (- num)))

   (*parser <Natural>)

   (*disj 3)
done))

