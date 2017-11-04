(load "pc.scm")
;path:/users/studs/bsc/2016/alonye/Downloads/comp181ass1-master/

;things to solve:
#|   > (test-string <Sexpr> "#\tabs")
Failure, Test result is: ((match  ) (remaining s))
The expected result was: (failed with report:)

> (test-string <Sexpr> " 13/")
Failure, Test result is: ((match 13) (remaining /))
The expected result was: ((match 13/) (remaining ))

> (test-string <Sexpr> " 13/ ")
Failure, Test result is: ((match 13) (remaining / ))
The expected result was: ((match 13/) (remaining ))
> (test-string <Sexpr> "( let ((x 2)
    (y 8))
    + x y   )")
Failure, Test result is: ((match (let ((x 2) (y 8)) + x y)) (remaining ))
The expected result was: ((match (+ (+ 9 (expt 4 3)) 6)) (remaining ))

> (test-string <Sexpr> "#()")
Failure, Test result is: ((match #()) (remaining ))
The expected result was: ((match (#())) (remaining ))

> (test-string <Sexpr> "#(1 2)")
Failure, Test result is: ((match #(1 2)) (remaining ))
The expected result was: ((match (#(1 2))) (remaining ))

> (test-string <Sexpr> "#((a b) #(1) 3)")
Failure, Test result is: ((match #((a b) #(1) 3)) (remaining ))
The expected result was: ((match (#((a b) (#(1)) 3))) (remaining ))

> (test-string <Sexpr> "`(the answer is: ##2 * 3 + 4 * 5)")
Failure, Test result is: (failed with report:)
The expected result was: ((match `(the answer is: (+ (* 2 3) (* 4 5)))) (remaining ))

> (test-string <Sexpr> " (let ((result ##a[0] + 2 * a[1] + 3 ^ a[2] - a[3] * b[i][j][i + j]))result)")
Failure, Test result is: ((match (let ((result (- (+ (+ (vector-ref a 0) (* 2 (vector-ref a 1))) (expt 3 (vector-ref a 2))) (* (vector-ref a 3) (vector-ref (vector-ref (vector-ref b i) j) (+ i j)))))) result)) (remaining ))
The expected result was: ((match (let ((result (- (+ (+ (vector-ref a 0) (* 2 (vector-ref a 1))) (expt 3 (vector-ref a 2))) (* (vector-ref a 3) (vector-ref (vector-ref (vector-ref b i) j) (+ i j)))))) result) (remaining )))


 |#


; ################## HELPER PARSERS




(define char->symbol
  (lambda (ch)
    (string->symbol (string ch))
))

(define <whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))

(define <line-comment>
  (let ((<end-of-line-comment>
   (new (*parser (char #\newline))
        (*parser <end-of-input>)
        (*disj 2)
        done)))
    (new (*parser (char #\;))
   
   (*parser <any-char>)
   (*parser <end-of-line-comment>)
   *diff *star

   (*parser <end-of-line-comment>)
   (*caten 3)
   done)))

(define ^<expression-comment>
  (lambda (<parser>)
  (new (*parser (word "#;"))
       (*delayed <parser>)
       (*caten 2)
       done)))

(define <comment>
  (lambda (<parser>)
  (disj (^<expression-comment> <parser>)
  <line-comment>
)))

(define <skip>
  (lambda (<parser>)
    (disj (<comment> <parser>)
    <whitespace>
)))

(define ^^<wrapped-with-comments>
  (lambda (<wrapper>)
  (lambda (<comment-parser>)
    (lambda (<p>)
      (new (*parser (star (<wrapper> <comment-parser>)))
     (*parser <p>)
     (*parser (star (<wrapper> <comment-parser>)))
     (*caten 3)
     (*pack-with
      (lambda (_left e _right) e))
     done)))))
     
(define ^^<wrapped>
  (lambda (<wrapper>) ;
    (lambda (<p>) ;gets parser and cleans all the whitspaces
      (new 
        (*parser <wrapper>)
        (*parser <p>)
        (*parser <wrapper>)
        (*caten 3)
        (*pack-with
         (lambda (_left e _right) e))
     done)))) 

(define <CleanSpaces>
  (lambda( <parser>)
    (new
      (*parser <whitespace>) *star
      (*parser <p>)
      (*parser <whitespace>) *star
      (*caten 3)
      *pack-with (lambda (leftSpace expression rightSpace) expression))
      ))

(define ^<skipped-with-infix-comment*> ((^^<wrapped-with-comments> <skip>) (lambda () <InfixExpression>)))
;(define ^<skipped-with-Sexpr-comment2*> ((^^<wrapped-with-comments> <skip>) (lambda () <Sexpr>)))
(define ^<skipped-with-Sexpr-comment*> ((^^<wrapped-with-comments> <skip>) (lambda () <Sexpr>)))
(define ^<skipped*> (^^<wrapped> (star <whitespace>)))


; Helper functions
; #####################################################################
(define <sexprWithSpace>                                                    
  (new

    (*delayed (lambda () <Sexpr>))
    (*parser (char #\space)) *star
    (*caten 2)
    (*pack-with (lambda (Sexpr space) Sexpr )) 

    (*parser (char #\space)) *star
    (*delayed (lambda () <Sexpr>)) 
    (*caten 2)
    (*pack-with (lambda (space Sexpr) Sexpr)) 

    (*delayed (lambda () <Sexpr>))

    (*disj 3)

    done))


(define <ParserWithSpaces>  
  (lambda(<parser>)                                                  
    (new

      (*delayed (lambda () <parser>))
      (*parser (char #\space)) *star
      (*caten 2)
      (*pack-with (lambda (Sexpr space) Sexpr )) 

      (*parser (char #\space)) *star
      (*delayed (lambda () <parser>))
      (*caten 2)
      (*pack-with (lambda (space Sexpr) Sexpr)) 

      (*delayed (lambda () <parser>))

      (*disj 3)

      done)))

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

(define list->symbol
  (lambda (lst)
    (string->symbol (list->string lst))
))

; #####################################################


;######################### CHAR ##########################

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


(define is-unicode
  (lambda (num) 
    (and
  (or (= num #x0) (> num #x0)) 
  (or (= num #x10FFFF) (< num #x10FFFF)))
))


(define <HexUnicodeChar> 
  (new
  (*parser (char-ci #\x))
  (*parser <HexChar>) *plus
  (*caten 2)

 (*pack-with (lambda (first rest)
   (list->hex-number `(,@rest))))
  (*only-if is-unicode)
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
      (*parser <VisibleSimpleChar>)
      *not-followed-by
    (*disj 3)

   (*caten 2)
   (*pack-with (lambda (c n) n))

done))

; ##################### String #########################

(define <StringHexChar>
  (new
  (*parser (word-ci "\\x"))
  (*parser <HexChar>) *star
  (*parser (char #\;))
  (*caten 3)
  (*pack-with (lambda (x hex semicolon)
      (list->hex-number `(,@hex))))
  (*only-if is-unicode)
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
    ((string-ci=? "\\t" str) #\tab)
    ((string-ci=? "\\f" str) #\page)
      ((string-ci=? "\\n" str) #\newline)
      ((string-ci=? "\\r" str) #\return)
      (else #f)))
    ))
done))


(define <StringLiteralChar>  
  (const (lambda(ch) (and (not(char=? #\\ ch)) (not(char=? #\" ch))))))

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



; ################## Symbol ##################
(define <SymbolChar>
  (new
   (*parser (range #\0 #\9))
   (*parser (range-ci #\a #\z))
   (*pack char-downcase)
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
   (*parser <SymbolChar>) *plus
   (*pack (lambda (lst)
 (list->symbol lst))) 
done))

;############## Boolean ####################
(define <Boolean> 
  (new
   (*parser (word-ci "#t"))
   (*pack (lambda (bool) #t))

   (*parser (word-ci "#f"))
   (*pack (lambda (bool) #f))

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
   (*pack-with (lambda (plus number) number))

   (*parser (char #\-))
   (*parser <Natural>)
   (*caten 2)
   (*pack-with (lambda (minus num) (- num)))

   (*parser <Natural>)

   (*disj 3)
done))

(define <Fraction>
  (new
   (*parser <Integer>)
   (*parser (char #\/))
   (*parser <Natural>)
   (*caten 3)

   (*pack-with (lambda (int backSlash num) (/ int num)))
  done))


(define <Number> 
  (disj <Fraction> <Integer> ))

(define <NumberNotFollowedBySymbol>
  (new
   (*parser <Number>)
   (*delayed (lambda () <Symbol>))
   (*parser (range #\0 #\9))
   (*parser (char #\/))
   (*disj 2)
   *diff
   *not-followed-by
  done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; InfixExtension ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
(define <InfixPrefixExtensionPrefix>
  (^<skipped*> (disj (word "##") (word "#%"))))


(define <InfixSymbol>
  (new
   (*parser (range #\0 #\9))
   (*parser (range-ci #\a #\z))
   (*pack char-downcase)
   (*parser (char #\!))
   (*parser (char #\$))
   (*parser (char #\_))
   (*parser (char #\=))
   (*parser (char #\<))
   (*parser (char #\>))
   (*parser (char #\?))
   (*disj 9)
   *plus
   (*pack list->symbol)
   done)) 
   
(define <AtomicInfixValue>
    (new   
      (*parser <Number>)
      (*parser <InfixSymbol>)
      (*parser (range #\0 #\9))
      *diff
      *not-followed-by
      
      (*parser <InfixSymbol>)
      
      (*delayed (lambda ()  <InfixSexprEscape>)) 
      
      (*disj 3)
    done))
    
(define <PowerSymbol>
  (new
    (*parser (^<skipped-with-infix-comment*> (char #\^)))
    (*parser (^<skipped-with-infix-comment*> (word "**")))  
    (*disj 2)  
    (*pack (lambda (_) 'expt))
   done))
  
(define <InfixParen>
  (new 
    (*parser (char #\())
    (*delayed (lambda () (^<skipped-with-infix-comment*> <InfixTree>)))
    (*parser (char #\)))
    (*caten 3)
    (*pack-with (lambda (s1 i s2) i))   
            
    (*parser (^<skipped-with-infix-comment*> <AtomicInfixValue>))
    (*disj 2)
  done)) 

(define <InfixArgList>
  (new
    (*delayed (lambda () <InfixExpression>))
    (*parser (char #\,))
    (*delayed (lambda () <InfixExpression>))   
    (*caten 2)
    (*pack-with (lambda (comma exp) exp))
    *star
    (*caten 2)
    (*pack-with (lambda (exp rest) `(,exp ,@rest)))
    
    (*parser <epsilon>)
    (*disj 2)
   done))  
     
(define <InfixArrayGetFuncall>
  (new           
    (*parser <InfixParen>)
    
    (*parser (^<skipped-with-infix-comment*> (char #\[)))
    (*delayed (lambda () <InfixExpression>))
    (*parser (^<skipped-with-infix-comment*> (char #\])))
    (*caten 3)
    (*pack-with (lambda (i1 s1 i2) (cons 'array-get s1)))
    
    (*parser (^<skipped*> (char #\()))
    (*parser <InfixArgList>)
    (*parser (^<skipped*> (char #\))))
    (*caten 3)
    (*pack-with (lambda (s1 args s2) (cons 'funcall args))) 
    
    (*disj 2)
    *star
    
    (*caten 2)
    (*pack-with (lambda (s1 s2)
  (if (null? s2)
  s1
        `,(fold-left (lambda (x y) 
        (if (equal? (car y) 'funcall)
        `(,x ,@(cdr y))
        `(vector-ref ,x ,(cdr y)))) s1 s2))))
        
    (*parser <InfixParen>)
    (*disj 2)
  done))      

(define <InfixNeg>
    (new
      (*parser (^<skipped*> <InfixArrayGetFuncall>))
      
      (*parser (char #\-))
      (*parser (^<skipped*> <InfixArrayGetFuncall>))
      (*caten 2)
      (*pack-with (lambda (sign exp) `(- ,exp)))
      
      (*disj 2)
    done))
  

   

  (define <InfixPow>
    (new                                        
      (*parser (^<skipped*> <InfixNeg>))
      (*parser (^<skipped*> <PowerSymbol>))
      (*caten 2)
      (*pack-with cons)
      *star
      
      (*parser (^<skipped*> <InfixNeg>))      
      
      (*caten 2)
      (*pack-with (lambda (rest last) 
        (if (null? rest)
          last
          (if (equal? (length rest) 1) `(,(cdar rest) ,(caar rest) ,last)
        `,(fold-left (lambda (x y) `(,(cdr y) ,(car y) ,x)) last (reverse rest))))))

      
     done)) 

(define <PlusMinus>
  (new
    (*parser (^<skipped-with-infix-comment*> (char #\+)))
    (*parser (^<skipped-with-infix-comment*> (char #\-)))
    (*disj 2)
    (*pack char->symbol)    
   done))
   
(define <MultDiv>
  (new
    (*parser (^<skipped-with-infix-comment*> (char #\*)))
    (*parser (^<skipped-with-infix-comment*> (char #\/)))
    (*disj 2)
    (*pack char->symbol)
   done))  


(define <InfixMulDiv>
    (new
      (*parser (^<skipped*> <InfixPow>))
      (*parser (^<skipped*> <MultDiv>))
      (*parser (^<skipped*> <InfixPow>))
      (*caten 3)
      (*pack-with (lambda (n1 sign n2) `(,sign ,n1 ,n2)))
        
      (*parser (^<skipped*> <MultDiv>))      
      (*parser (^<skipped*> <InfixPow>))
      (*caten 2)
      (*pack-with cons)
      *star
      
      (*caten 2)
      (*pack-with (lambda (n1 n2) 
  (if (null? n2)
    n1
  `,(fold-left (lambda (x y) `(,(car y) ,x ,(cdr y))) n1 n2))))
      
      (*parser (^<skipped*> <InfixPow>))
      (*disj 2)  
done))

   
  
(define <InfixAddSub>
    (new
      (*parser (^<skipped*> <InfixMulDiv>))
      (*parser (^<skipped*> <PlusMinus>))
      (*parser (^<skipped*> <InfixMulDiv>))
      (*caten 3)
      (*pack-with (lambda (n1 sign n2) `(,sign ,n1 ,n2)))
        
      (*parser (^<skipped*> <PlusMinus>))      
      (*parser (^<skipped*> <InfixMulDiv>))
      (*caten 2)
      (*pack-with cons)
      *star
      
      (*caten 2)
      (*pack-with (lambda (n1 n2) 
  (if (null? n2)
    n1
  `,(fold-left (lambda (x y) `(,(car y) ,x ,(cdr y))) n1 n2))))
      
      (*parser (^<skipped*> <InfixMulDiv>))
      (*disj 2)  
done))
     
   



(define <InfixTree>
  (new
    (*parser (^<skipped*> <InfixAddSub>))
  done))
  
  
(define <InfixSexprEscape>
  (new
    (*parser (^<skipped*> <InfixPrefixExtensionPrefix>))
    (*delayed (lambda () (^<skipped-with-Sexpr-comment*> <Sexpr>)))
    (*caten 2)
    (*pack-with (lambda (i w) w))
  done))
    
(define <InfixExpression> (^<skipped-with-infix-comment*> <InfixTree>))
   
   
(define <InfixExtension>
  (new 
    (*parser <InfixPrefixExtensionPrefix>)  
    (*parser <InfixExpression>)
    (*caten 2)
    (*pack-with (lambda (prefix exp) exp))    
done))
     ;######## end of infix ###############################################
 

  (define <ProperList>
  (new
    (*parser (word "("))
    (*parser <sexprWithSpace>) *star
    (*parser (word ")"))
    (*caten 3)
    (*pack-with (lambda(left_br Sexpr right_br) Sexpr ))
    done)) 

     
(define <ImproperList>
  (new
    (*parser (word "("))
    (*parser <sexprWithSpace>) *plus
    (*parser (word ".") )
    (*delayed (lambda () <sexprWithSpace>))
    (*parser (word ")"))
    (*caten 5)
    (*pack-with (lambda(left_br sexpr1 dot sexpr2 right_br ) `(,@sexpr1 . ,sexpr2)))
    done)) 
  
     
(define <Vector>
  (new
    (*parser (word "#("))
    (*parser <sexprWithSpace>) *star
    (*parser (word ")"))
    (*caten 3)
    (*pack-with (lambda(left_br Sexpr right_br ) `#(,@Sexpr) ))
      done))
  
     
(define <Quoted>
  (new
    (*parser (word "'"))
    (*delayed (lambda () <Sexpr>))
    (*caten 2)
    (*pack-with (lambda(quot Sexpr) `'(,@Sexpr) ))
      done))
  
     
(define <QuasiQuoted>
  (new
    (*parser (word "`"))
    (*delayed (lambda () <Sexpr>))
    (*caten 2)
    (*pack-with (lambda(qq Sexpr)  (list 'quasiquote Sexpr)))
      done))
  
     
(define <Unquoted>
  (new
    (*parser (word ","))
    (*parser (word "@"))
    *not-followed-by
    (*delayed (lambda () <Sexpr>)) 
    (*caten 2)
    (*pack-with (lambda(unquot Sexpr) (list 'unquote Sexpr) ))
      done))
 
     
(define <UnquoteAndSpliced>
  (new
    (*parser (word ",@"))
    (*delayed (lambda () <Sexpr>)) 
    (*caten 2)
    (*pack-with (lambda(unquot Sexpr) (list 'unquote-splicing Sexpr)) )
      done)) 
     
     
(define <CBNameSyntax1>
  (new
    (*parser (word "@"))
    (*delayed (lambda () <Sexpr>))
    (*caten 2)
    (*pack-with (lambda(shtrudel Sexpr) (list 'cbname Sexpr)  ))
      done)) 
     
     
  (define <CBNameSyntax2>
  (new
    (*parser (word "{"))
    (*delayed (lambda () <Sexpr>))
    (*parser (word "}"))
    (*caten 3)
    (*pack-with (lambda(left_br Sexpr right_br) (list 'cbname Sexpr) ))
    done))  

     
(define <CBName>
  (disj <CBNameSyntax1> <CBNameSyntax2>))


(define <Sexpr> 
  (^<skipped-with-Sexpr-comment*>
    (disj <Boolean>
          <Char>
          <NumberNotFollowedBySymbol>
          <String>
          <Symbol>
          <ProperList>
          <ImproperList>
          <Vector>
          <Quoted>
          <QuasiQuoted>
          <Unquoted>
          <UnquoteAndSpliced>
          <CBName>
          <InfixExtension>)))

(define ^<skipped-with-sexpr-comment*> ^<skipped-with-Sexpr-comment*>)
(define <sexpr> <Sexpr>)




