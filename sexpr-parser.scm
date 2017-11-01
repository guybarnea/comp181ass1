(load "pc.scm")

;#Path= /users/studs/bsc/2016/alonye/Downloads/comp181ass1-master/
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

(define <Sexpr>
	(new
		(*parser <Boolean>)
		(*parser <Char>)
		(*parser <Number>)
		(*parser <String>)
		(*parser <Symbol>)
		(*parser <ProperList>)
		(*parser <ImproperList>)
		(*parser <Vector>)
		(*parser <Quoted>)
		(*parser <QuasiQuoted>)
		(*parser <Unquoted>)
		(*parser <UnquoteAndSpliced>)
		(*parser <CBName>)
		(*parser <InfixExtension>)
		(*disj 14)
	done))

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

(define <Char>
	(new
		(*parser <CharPrefix>)
		(*parser <VisibleSimpleChar>) 
		(*parser <NamedChar>) 
		(*parser <HexUnicodeChar>) 
	done)) 

