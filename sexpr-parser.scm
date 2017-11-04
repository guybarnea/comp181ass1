(load "pc.scm")
;path:/users/studs/bsc/2016/alonye/Downloads/comp181ass1-master/

; ################## HELPER PARSERS

; unused funtions:

#| (define ^<meta-char>
  (lambda (str ch)
    (new (*parser (word str))
   (*pack (lambda (_) ch))
   done)))   |# 

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
(define ^<skipped-with-sexpr-comment*> ((^^<wrapped-with-comments> <skip>) (lambda () <sexpr>)))
(define ^<skipped*> (^^<wrapped> (star <whitespace>)))


; Helper functions
; #####################################################################
(define <sexprWithSpace>                                                    
  (new

    (*delayed (lambda () <Sexpr>))
    (*parser (char #\space)) *star
    (*caten 2)
    (*pack-with (lambda (sexpr space) sexpr )) 

    (*parser (char #\space)) *star
    (*delayed (lambda () <Sexpr>)) 
    (*caten 2)
    (*pack-with (lambda (space sexpr) sexpr)) 

    (*delayed (lambda () <Sexpr>))

    (*disj 3)

    done))


(define <ParserWithSpaces>  
  (lambda(<parser>)                                                  
    (new

      (*delayed (lambda () <parser>))
      (*parser (char #\space)) *star
      (*caten 2)
      (*pack-with (lambda (sexpr space) sexpr )) 

      (*parser (char #\space)) *star
      (*delayed (lambda () <parser>))
      (*caten 2)
      (*pack-with (lambda (space sexpr) sexpr)) 

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

;############## InfixExtension ####################
(define <InfixPrefixExtensionPrefix>
(disj (word "##") (word "#%")))

#| (define notInfixSymbol
  (lambda (str)
  (not (or (string-ci=? (list->string str) "+")  (string-ci=? (list->string str) "-") (string-ci=? (list->string str) "*") 
  (string-ci=? (list->string str) "//") (string-ci=? (list->string str) "**") (string-ci=? (list->string str) "^"))))) |#

(define <InfixSexprEscape>
  (new
   (*parser <InfixPrefixExtensionPrefix>)
   (*delayed (lambda () <Sexpr>))
   (*caten 2)
done
))

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

#| 
   (*parser <Symbol>)
   (*only-if notInfixSymbol) |#
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


(define <InfixParen>
  (new
   (*parser (word "("))
   (*delayed (lambda () (^<skipped-with-infix-comment*> <InfixExpression>)))
   (*parser (word ")"))  
   (*caten 3)
   (*pack-with (lambda (_ exp ._)  exp))

   (*parser (^<skipped-with-infix-comment*> <AtomicInfixValue>))
   (*disj 2)
done
))


(define <InfixArgList>
  (new
    (*delayed (lambda () <InfixExpression>))
    (*parser (char #\,))
    (*delayed (lambda () <InfixExpression>))   
    (*caten 2)
    (*pack-with (lambda (_ str) str))
    *star
    (*caten 2)
    (*pack-with (lambda (first second) `(,first ,@second)))
    
    (*parser <epsilon>)
    (*disj 2)
done)) 

(define <InfixFuncall>
  (new
   (*parser <InfixParen>)

    (*parser (^<skipped*> (word "(")))
    (*parser <InfixArgList>)
    (*parser (^<skipped*> (word ")")))
    (*caten 3)
    (*pack-with (lambda (_ exp n) exp))
    *star
    (*caten 2)
    (*pack-with (lambda (s1 s2)
        (if (null? s2)
          s1
        `,(fold-left (lambda (x y) `(,x ,@y)) s1 s2))))

    (*parser <InfixParen>)
    (*disj 2)
done
))


(define <InfixArrayGet>
  (new
   (*parser <InfixFuncall>)

    (*parser (^<skipped-with-infix-comment*> (char #\[)))
    (*delayed (lambda () <InfixExpression>))   
    (*parser (^<skipped-with-infix-comment*> (char #\])))
    (*caten 3)
    (*pack-with (lambda (_ exp n) exp))
    *star
    (*caten 2)
    (*pack-with (lambda (s1 s2)
        (if (null? s2)
          s1
        `,(fold-left (lambda (x y) `(vector-ref ,x ,y)) s1 s2))))

    (*parser <InfixFuncall>)
    (*disj 2)
done
))

(define <InfixNeg>
    (new 
      (*parser (^<skipped*> <InfixArrayGet>))
      (*parser (char #\-))
      (*parser (^<skipped*> <InfixArrayGet>))
      (*caten 2)
      (*pack-with (lambda (_ first) `(- ,first))) 

      (*disj 2)
done))

(define <InfixPow>
 (new
   (*parser (^<skipped*> <InfixNeg>))
   (*parser (^<skipped*> <PowerSymbol>))
;   (*parser <Number>)
   (*caten 2)
   (*pack-with cons);(lambda (first _ second) first))
   *star
   (*parser (^<skipped*> <InfixNeg>))


      (*caten 2)
      (*pack-with (lambda (rest last)
        (if (null? rest)
          last
          (if (equal? (length rest) 1) `(expt ,(caar rest) ,last)
        `,(fold-left (lambda (x y) `(expt,(car y) ,x)) last (reverse rest))))))

  done))
#| 
   (*parser <PowerSymbol>)
   (*parser <Number>)
   (*caten 2)
   (*pack-with (lambda (first second) second))
   *star


   (*caten 2)
   (*pack-with (lambda (first second) 
   (if (null? second)
    first
   `,(fold-left (lambda (x y) `(expt ,x ,y)) first second))))

   (*parser <Number>)
   (*parser <InfixSymbol>)
   (*disj 3) |#
#| done
)) |#

(define <InfixDiv>
 (new
   (*parser (^<skipped*> <InfixPow>))
   (*parser (^<skipped*> (char #\/)))
   (*parser (^<skipped*> <InfixPow>))
   (*caten 3)
   (*pack-with (lambda (first _ second) `(/ ,first ,second)))

   (*parser (^<skipped*> (char #\/)))
   (*parser (^<skipped*> <InfixPow>))
   (*caten 2)
   (*pack-with (lambda (first second) second))
   *star


   (*caten 2)
   (*pack-with (lambda (first second) 
   (if (null? second)
    first
   `,(fold-left (lambda (x y) `(/ ,x ,y)) first second))))

   (*parser (^<skipped*> <InfixPow>))
   (*disj 2)
done
))

 (define <InfixMul>
 (new
   (*parser (^<skipped*> <InfixDiv>))
   (*parser (^<skipped*> (char #\*)))
   (*parser (^<skipped*> <InfixDiv>))
   (*caten 3)
   (*pack-with (lambda (first _ second) `(* ,first ,second)))

   (*parser (^<skipped*> (char #\*)))
   (*parser (^<skipped*> <InfixDiv>))
   (*caten 2)
   (*pack-with (lambda (first second) second))
   *star


   (*caten 2)
   (*pack-with (lambda (first second) 
   (if (null? second)
    first
   `,(fold-left (lambda (x y) `(* ,x ,y)) first second))))

   (*parser (^<skipped*> <InfixDiv>))
   (*disj 2)
done
))



(define <InfixAdd>
  (new
   (*parser (^<skipped*> <InfixMul>))
   (*parser (^<skipped*> (char #\+)))
   (*parser (^<skipped*> <InfixMul>))
   (*caten 3)
   (*pack-with (lambda (first _ second) `(+ ,first ,second)))

   (*parser (^<skipped*> (char #\+)))
   (*parser (^<skipped*> <InfixMul>))
   (*caten 2)
   (*pack-with (lambda (first second) second))
   *star


   (*caten 2)
   (*pack-with (lambda (first second) 
   (if (null? second)
    first
   `,(fold-left (lambda (x y) `(+ ,x ,y)) first second))))

   (*parser (^<skipped*> <InfixMul>))
   (*disj 2)
done
))

(define <InfixSub>
  (new
   (*parser (^<skipped*> <InfixAdd>))
   (*parser (^<skipped*>(char #\-)))
   (*parser (^<skipped*> <InfixAdd>))
   (*caten 3)
   (*pack-with (lambda (first _ second) `(- ,first ,second)))

   (*parser (^<skipped*>(char #\-)))
   (*parser (^<skipped*> <InfixAdd>))
   (*caten 2)
   (*pack-with (lambda (first second) second))
   *star


   (*caten 2)
   (*pack-with (lambda (first second) 
   (if (null? second)
    first
   `,(fold-left (lambda (x y) `(- ,x ,y)) first second))))

   (*parser (^<skipped*> <InfixAdd>))
   (*disj 2)
done
))


(define  <InfixExpression>
(new
(*parser (^<skipped*> <InfixSub>))
done)) 
 
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
    (*pack-with (lambda(left_br sexpr right_br) sexpr ))
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
    (*pack-with (lambda(left_br sexpr right_br ) `#(,@sexpr) ))
      done))
  
     
(define <Quoted>
  (new
    (*parser (word "'"))
    (*delayed (lambda () <Sexpr>))
    (*caten 2)
    (*pack-with (lambda(quot sexpr) `'(,@sexpr) ))
      done))
  
     
(define <QuasiQuoted>
  (new
    (*parser (word "`"))
    (*delayed (lambda () <Sexpr>))
    (*caten 2)
    (*pack-with (lambda(qq sexpr)  (list 'quasiquote sexpr)))
      done))
  
     
(define <Unquoted>
  (new
    (*parser (word ","))
    (*delayed (lambda () <Sexpr>)) 
    (*caten 2)
    (*pack-with (lambda(unquot sexpr) (list 'unquote sexpr) ))
      done))
 
     
(define <UnquoteAndSpliced>
  (new
    (*parser (word ",@"))
    (*delayed (lambda () <Sexpr>)) 
    (*caten 2)
    (*pack-with (lambda(unquot sexpr) (list 'unquote-splicing sexpr)) )
      done)) 
     
     
(define <CBNameSyntax1>
  (new
    (*parser (word "@"))
    (*delayed (lambda () <Sexpr>))
    (*caten 2)
    (*pack-with (lambda(shtrudel sexpr) (list 'cbname sexpr)  ))
      done)) 
     
     
  (define <CBNameSyntax2>
  (new
    (*parser (word "{"))
    (*delayed (lambda () <Sexpr>))
    (*parser (word "}"))
    (*caten 3)
    (*pack-with (lambda(left_br sexpr right_br) (list 'cbname sexpr) ))
    done))  

     
(define <CBName>
  (disj <CBNameSyntax1> <CBNameSyntax2>))


(define <Sexpr> 
  (^<skipped-with-sexpr-comment*>
    (disj <Boolean>
          <Char>
          <String>
          <NumberNotFollowedBySymbol>
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
