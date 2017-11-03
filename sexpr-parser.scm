(load "pc.scm")


;(define <sexpr>
;    (disj <Boolean> <Char> <String> <Number> <Symbol>
;     <ProperList> <ImproperList> <Vector> <Quoted> <QuasiQuoted> <Unquoted> 
;	  <UnquoteAndSpliced> <CBName>  <InfixExtension> 
;))
;path:/users/studs/bsc/2016/alonye/Downloads/comp181ass1-master/

(define is-unicode
  (lambda (num) 
    (and
	(or (= num #x0) (> num #x0)) 
	(or (= num #x10FFFF) (< num #x10FFFF)))
))

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

;######### CHAR #############

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

(define <VisibleSimpleChar> (const (lambda (ch) (char<=? (integer->char 33) ch))))

; DELETE? IS-UNICODE
(define is-unicode
  (lambda (num) 
    (and
	(or (= num #x0) (> num #x0)) 
	(or (= num #x10FFFF) (< num #x10FFFF)))
))


(define <HexUnicodeChar> ;;learn about this parser
  (new
  (*parser (char-ci #\x))
  (*parser <HexChar>) *plus
  (*caten 2)

 (*pack-with (lambda (first rest)
   (list->hex-number `(,@rest))))
  (*pack integer->char)  
done))


(define <Char>
  (new
   (*parser <CharPrefix>)

   (*parser <NamedChar>)
   (*pack (lambda (str)
    (cond  ((string-ci=? "lambda" str) (integer->char 955))
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
   (*disj 3)

   (*caten 2)
   (*pack-with (lambda (c n) n))
done))

;################# String ###############

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
   (*parser (word-ci "\\\\")) ; \\
   (*parser (word-ci "\\\"")) ; \"
   (*parser (word-ci "\\t"))  ; \t
   (*parser (word-ci "\\f"))  ; \f
   (*parser (word-ci "\\n"))  ; \n
   (*parser (word-ci "\\r"))  ; \r
   (*disj 6)
   (*pack (lambda(meta-ch)
   	(let ((str (list->string meta-ch)))
   	  (cond 
   	  ((string-ci=? "\\\\" str) #\\)
	  ((string-ci=? "\\\"" str) #\")
	  ((string-ci=? "\\t" str) #\t)
	  ((string-ci=? "\\f" str) #\f)
      ((string-ci=? "\\n" str) #\n)
      ((string-ci=? "\\r" str) #\r)
      (else #f)))
   	))
done))

(define <StringLiteralChar> (const (lambda(ch) (and (not(char=? #\\ ch)) (not(char=? #\" ch))))))

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

(define <StringHexChar>
  (new
  (*parser (word-ci "\\x"))
  (*parser <HexChar>)
  *star
  (*parser (char #\;))
  (*caten 3)
  (*pack-with (lambda (x hex semicolon)
		  (list->hex-number `(,@hex))))
  (*only-if is-unicode)
  (*pack integer->char)			    
done))

;(define <InfixExtension>
  ;(new 
   ; (*parser <InfixPrefixExtensionPrefix>)  
    ;(*parser <InfixExpression>)
   ; (*caten 2)
  ;  (*pack-with (lambda (prefix exp) exp))    
;done))




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

(define <Symbol> 
  (new 
   (*parser <SymbolChar>)
   *plus
   (*pack (lambda (lst)
 (list->string lst))) 
done))






;############## Boolean ####################
(define <Boolean> 
  (new
   (*parser (word-ci "#t"))
   (*pack (lambda (_) #t))

   (*parser (word-ci "#f"))
   (*pack (lambda (_) #f))

   (*disj 2)
done))

;############## NUMBER ####################
(define <Natural>
  (new
   (*parser (range #\0 #\9)) *plus
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
   (*pack-with (lambda (_ num) (- num)))

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


(define <Number> (disj <Fraction> <Integer> ))



;############## InfixExtension ####################

(define <InfixPrefixExtensionPrefix>
(disj (word "##") (word "#%")))

(define notInfixSymbol
	(lambda (str)
	(not (or (string-ci=? str "+")  (string-ci=? str "-") (string-ci=? str "*") 
	(string-ci=? str "//") (string-ci=? str "**") (string-ci=? str "^")))))

(define <InfixSymbol>
  (new
   (*parser <Symbol>)
   (*only-if notInfixSymbol)
done
))

(define <PowerSymbol>
  (new
   (*parser (word "**"))
   (*parser (word "^"))
   (*disj 2)
   (*pack (lambda (word)
   	(list->string word)))
done))

(define <InfixAdd>
  (new
   (*parser <Natural>)
   (*parser (char #\+))
   (*parser <Natural>)
   (*caten 3)
   (*pack-with (lambda (first _ second) (+ first second)))
done
))

; Helper function
; #####################################################################
(define <sexprWithSpace>                                                    
  (new

    (*parser <Sexpr>)
    (*parser (char #\space))
    (*caten 2)
    (*pack-with (lambda (sexpr space) sexpr )) 

    (*parser (char #\space))
    (*parser <Sexpr>)
    (*caten 2)
    (*pack-with (lambda (space sexpr) sexpr)) 

    (*parser <Sexpr>)

    (*disj 3)

    done))
; #####################################################################

  (define <ProperList>
  (new
    (*parser (word "("))
    (*parser <sexprWithSpace>) *star
    (*parser (word ")"))
    (*caten 3)
    (*pack-with (lambda(left_br sexpr right_br) sexpr ))
    done)) 

(define <ImproperList>
  (new
    (*parser (word "("))
    (*parser <sexprWithSpace>) *plus
    (*parser (word ".") )
    (*parser <Sexpr>)
    (*parser (word ")"))
    (*caten 5)
    (*pack-with (lambda(left_br sexpr1 dot sexpr2 right_br ) `(,@sexpr1 . ,sexpr2)))
    done)) 
