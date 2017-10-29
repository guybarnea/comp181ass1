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
(define list->decimal-number (list->number 10))

(define <HexChar>
  (new
   (*parser (range #\0 #\9))
   (*parser (range-ci #\a #\f))
   (*disj 2)
  done))


(define <CharPrefix> (word "#\\"))

(define <NamedChar>
  (new
   (*parser (word-ci "lambda"))
   (*parser (word-ci "newline"))
   (*parser (word-ci "nul"))
   (*parser (word-ci "page"))
   (*parser (word-ci "return"))
   (*parser (word-ci "space"))
   (*parser (word-ci "tab"))   
   (*disj 7)
   (*pack list->string)
done))

(define <VisibleSimpleChar>
	(const (lambda (ch) (char<=? (integer->char 33) ch))))
	;(new 
	;(*parser <any-char>)
	;(*parser (range (integer->char 33) (integer->char 255)))
	;(*pack (lambda (ch) (string->char ch)))
;done))


(define is-unicode
  (lambda (num) 
    (and
	(or (= num #x0) (> num #x0)) 
	(or (= num #x10FFFF) (< num #x10FFFF)))
))

(define <HexUnicodeChar> ;;learn about this parser
  (new
  (*parser (char-ci #\x))
  (*parser <HexChar>)
  *plus
  (*caten 2)

(*pack-with (lambda (c h)
   (list->hex-number `(,@h))))
 ; (*guard is-unicode)
  (*pack integer->char)  
done))


(define <Char>
  (new
   (*parser <CharPrefix>)
   (*parser <NamedChar>)
   (*pack (lambda (str)
    (cond ((string-ci=? "lambda" str) (integer->char 955))
	  ((string-ci=? "newline" str) #\newline)
	  ((string-ci=? "nul" str) #\nul)
	  ((string-ci=? "page" str) #\page)
          ((string-ci=? "return" str) #\return)
          ((string-ci=? "space" str) #\space)
          ((string-ci=? "tab" str) #\tab)
          (else #f))
	))

   (*parser <HexUnicodeChar>)

   (*parser <VisibleSimpleChar>)
   ;(*parser <VisibleSimpleChar>)
   ;(*parser (char #\)))
   ;(*parser (char #\]))
   ;(*parser (char #\[))
   ;(*parser (char #\())
   ;(*disj 4)
   ;*diff
   ;*not-followed-by

   (*disj 3)
   (*caten 2)
   (*pack-with (lambda (c n) n))
done))

(define <SymbolChar>
  (new
   (*parser (range #\0 #\9))
   (*parser (range-ci #\a #\z))
   ;(*pack char-downcase)
   (*parser (char #\!))
   (*parser (char #\$))
   (*parser (char #\^))
   (*parser (char #\*))
   (*parser (char #\-))
   (*parser (char #\_))
   (*parser (char #\=))
   (*parser (char #\+))
   (*parser (char #\<))
   (*parser (char #\>))
   (*parser (char #\?))
   (*parser (char #\/))
   (*disj 14)
done))

(define <StringHexChar>
  (new
  (*parser (word-ci "\\x"))
  (*parser <HexChar>)
  *star
  (*parser (char #\;))
  (*caten 3)
  (*pack-with (lambda (x hex semicolon)
		  (list->hex-number `(,@hex))))
  (*pack integer->char)			    
done))

(define <StringMetaChar>
  (new
   (*parser (word-ci "\\\\"))
   (*parser (word-ci "\\\""))
   (*parser (word-ci "\\t"))
   (*parser (word-ci "\\f"))
   (*parser (word-ci "\\n"))
   (*parser (word-ci "\\r"))
  ; (*parser (^<meta-char> "\\\\" #\\))
  ; (*parser (^<meta-char> "\\\"" #\"))
  ; (*parser (^<meta-char> "\\t" #\tab))
  ; (*parser (^<meta-char> "\\f" #\page))
  ;(*parser (^<meta-char> "\\n" #\newline))
  ; (*parser (^<meta-char> "\\r" #\return))
   (*disj 6)
   (*pack (lambda(meta-ch)
   	(let ((str (list->string meta-ch)))
   	  (cond 
   	  ((string-ci=? "\\\\" str) #\\)
	  ((string-ci=? "\\\"" str) #\")
	  ((string-ci=? "\\t" str) #\tab)
	  ((string-ci=? "\\f" str) #\page)
      ((string-ci=? "\\n" str) #\newline)
      ((string-ci=? "\\r" str) #\return)
      (else #f)))
   	))
done))

(define <StringLiteralChar>
	(const (lambda (ch) (not (char=? #\\ ch)))))
 ; (new
    ;(*parser <any-char>)
   ; (*parser (char #\\))
    ;*diff
   ;*not-followed-by

;done))

(define <StringChar> 
	(disj <StringHexChar> <StringMetaChar> <StringLiteralChar>))

(define <String>
  (new
   (*parser (char #\"))
   (*parser <StringChar>)
   (*parser (char #\"))
   *diff
   *star
   (*parser (char #\"))
   (*caten 3)
   (*pack-with (lambda (open str close)
(list->string str)))
done)) 

;(define <InfixExtension>
  ;(new 
   ; (*parser <InfixPrefixExtensionPrefix>)  
    ;(*parser <InfixExpression>)
   ; (*caten 2)
  ;  (*pack-with (lambda (prefix exp) exp))    
;done))


(define <InfixPrefixExtensionPrefix>
(disj (word "##") (word "#%")))


(define <Symbol> 
  (new 
   (*parser <SymbolChar>)
   *plus
   (*pack (lambda (lst)
	(string->symbol (list->string lst)))) 
done))

(define <InfixSymbol>
  (new
   (*parser <Symbol>)
   (*parser (char #\+)) 
   *diff
   (*parser (char #\-)) 
   *diff
   (*parser (char #\*)) 
   *diff
   (*parser (word "**")) 
   *diff
   (*parser (char #\^)) 
   *diff
   (*parser (char #\/)) 
   *diff
done))

(define <check>
(new
(*parser (range #\2 #\9))
(*parser (range #\2 #\9))
*not-followed-by
done))

(define <Boolean> 
  (new
   (*parser (word-ci "#t"))
   (*pack (lambda (_) #t))

   (*parser (word-ci "#f"))
   (*pack (lambda (_) #f))
   (*disj 2)
done))




(define <Natural>
  (new
   (*parser (range #\0 #\9))
   *plus
   (*pack list->decimal-number)
done))

(define <Integer>
  (new
   (*parser (char #\+))
   (*parser <Natural>)
   (*caten 2)
   (*pack-with (lambda (_ number) number))

   (*parser (char #\-))
   (*parser <Natural>)
   (*caten 2)
   (*pack-with (lambda (_ num) (- number)))

   (*parser <Natural>)

   (*disj 3)
done))


(define <Fraction>
  (new
   (*parser <Integer>)
   (*parser (char #\/))
   (*parser <Natural>)
   (*caten 3)

   (*pack-with (lambda (int _ num) (/ int num)))
  done))