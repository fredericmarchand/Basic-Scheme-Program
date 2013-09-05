#lang racket
;;;  Comp3007 Assignment 1
;;;
;;;  Frederic Marchand
;;;  ID# 100817579




;;;  Question 1   
(display "Question 1")
(newline)
;;;  a)
;;;  Operation: (2*4)+((3*5+6*7)*9)
;;;  Output should be 521
(+ (* 2 4) (* (+ (* 3 5) (* 6 7)) 9))


;;;  b)
;;;  Operation : (-1+((3+(4*2)-6)*3)/(7*2))
;;;  Output should be 0.0714 or 1/14
(- (/ (* (+ 3 (* 4 2) -6) 3) (* 7 2)) 1)


;;; -----------------------------------------------------------------------------------------------------


;;;  Question 2
(newline)
(display "Question 2")
(newline)
;;;  Recursive
(define (f-recursive n)
  (if (< n 4) ;base case
      n
     (+ (f-recursive (- n 1)) (* (f-recursive (- n 2)) 2) (* (f-recursive (- n 3)) 3) (* (f-recursive (- n 4)) 4)) ;recursive step
     )
  )

;;  Testing
(f-recursive 0)  ;;  Input: 0  ->  Expected output: 0
(f-recursive 1)  ;;  Input: 1  ->  Expected output: 1
(f-recursive 2)  ;;  Input: 2  ->  Expected output: 2
(f-recursive 3)  ;;  Input: 3  ->  Expected output: 3
(f-recursive 4)  ;;  Input: 4  ->  Expected output: 10
(f-recursive 5)  ;;  Input: 5  ->  Expected output: 26
(f-recursive 6)  ;;  Input: 6  ->  Expected output: 63


;;;  Iterative
(newline)(newline)

;this function calls the iterative version of the question
(define (f n)
  (if (< n 4) ;base case
      n
      (f-iter 3 2 1 0 (- n 3)))) 

;helper function
(define (f-iter a b c d total)
    (if (= total 1) ;base case
        (+ a (* 2 b) (* 3 c) (* 4 d))
        (f-iter (+ a (* 2 b) (* 3 c) (* 4 d)) a b c (- total 1)))) ;tail recursion
        

;;  Testing
(f 0)  ;;  Input: 0  ->  Expected output: 0
(f 1)  ;;  Input: 1  ->  Expected output: 1
(f 2)  ;;  Input: 2  ->  Expected output: 2
(f 3)  ;;  Input: 3  ->  Expected output: 3
(f 4)  ;;  Input: 4  ->  Expected output: 10
(f 5)  ;;  Input: 5  ->  Expected output: 26
(f 6)  ;;  Input: 6  ->  Expected output: 63


;;; -----------------------------------------------------------------------------------------------------


;;;  Question 3  
(newline)
(display "Question 3")
(newline)

; row |
;-----|
;  0  |       1
;  1  |      1 1
;  2  |     1 2 1
;  3  |    1 3 3 1
;  4  |   1 4 6 4 1

;;  The variable 'idx' is the index within the row  (0, 1, 2, 3, 4, ...)
;;  The row values and index values start at 0

(define (pascal row idx)
  (cond ((> idx row) (display "Invalid")(newline))
        ((= idx 0) 1)
        ((= row idx) 1)
        (else (+ (pascal (- row 1) (- idx 1)) (pascal (- row 1) idx)))))
  
;;  Testing
;;  Input (x, y) --> Expected value: value in pascal's triangle or Invalid message if out of bounds
(pascal 4 2)  ;;Expected output: 6
(pascal 5 5)  ;;Expected output: 1
(pascal 3 2)  ;;Expected output: 3
(pascal 0 0)  ;;Expected output: 1
(pascal 1 2)  ;;Expected output: Invalid because idx 2 in row 1 does not exist


;;; -----------------------------------------------------------------------------------------------------


;;;  Question 4   
(newline)
(display "Question 4")
(newline)

(define (exponentiation b n)
  (expt-iter b n b))
   
;;helper function
(define (expt-iter b n value)
  (if (< (/ n 2) 1);base case
         value
         (expt-iter b (- n 1) (* value b))));tail recursion


;Testing
(exponentiation 3 2)  ;; Expected Output:  9
(exponentiation 2 2)  ;; Expected Output:  4
(exponentiation 3 3)  ;; Expected Output:  27
(exponentiation 4 4)  ;; Expected Output:  256
(exponentiation 2 5)  ;; Expected Output:  32
(exponentiation 6 3)  ;; Expected Output:  216


;;; -----------------------------------------------------------------------------------------------------


;Question 5  
(newline)
(display "Question 5")
(newline)

(define (multiply a b)
  (cond ((or (= a 0) (= b 0)) 0)
        (else (= b 1)
              a
              (+ a (multiply a (- b 1)))
              )))
  
;Testing
(multiply 3 1)   ;;Expected Output:  3
(multiply 3 2)   ;;Expected Output:  6
(multiply 4 3)   ;;Expected Output:  12
(multiply 5 5)   ;;Expected Output:  25
(multiply 1 1)   ;;Expected Output:  1
(multiply 0 1)   ;;Expected Output:  0
(multiply 5 0)   ;;Expected Output:  0


;Substitution model for (multiply 2 3)
(multiply 2 3)  ;;result = 6
; 2 + (multiply(2 2))      ;recurse
; 2 + 2 + (multiply(2 1))  ;base case
; 2 + 2 + 2 = 6            ;result


;;; -----------------------------------------------------------------------------------------------------


;Question 6  
(newline)
(display "Question 6")
(newline)

(define (multiply-iter x y)
  
  ;;this procedure doubles a number
  (define (double a)
    (+ a a))
  
  ;;this function divides a number by 2
  (define (half b)
    (/ b 2))
  
  ;;this function performs a multiplication by doubling, halving and adding
  (define (multiply x y z)
    (cond ((= 0 y) z)
          ((even? y) (multiply (double x) (half y) z))
          (else (multiply x (- y 1) (+ x z)))))
  
  (multiply x y 0))

;;Testing
(multiply-iter 2 3)    ;;Expected Output:  6
(multiply-iter 5 3)    ;;Expected Output:  15
(multiply-iter 7 4)    ;;Expected Output:  28
(multiply-iter 5 5)    ;;Expected Output:  25
(multiply-iter 0 3)    ;;Expected Output:  0
(multiply-iter 1 0)    ;;Expected Output:  0

;Substitution model for (multiply-iter 2 3)
; (multiply 2 3 0)   ;; plug in values to multiply function
; (multiply 2 2 2)   ;;else case since y is not even
; (multiply 4 1 2)   ;;y is even so use first case
; (multiply 4 0 6)   ;;else case since y is not even
; Base case: y = 0 so output z which is equal to 6


;;; -----------------------------------------------------------------------------------------------------


;Question 7   
(newline)
(display "Question 7")
(newline)


(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))


;(test 0 (p))

;Since Applicative Order Evaluation evaluates all parameters before entering the test function,
;it goes into an infinite loop because (p) is an infinitly recursive function.

;For Normal Order Evaluation, the test function will be entered without evaluating (p) and return 0 
;and never go to the else case unless x was a value other than 0.



;;; -----------------------------------------------------------------------------------------------------


;Question 8  
(newline)
(display "Question 8")
(newline)


(define (my-cbrt x)
  
  ;;this function adds 2 numbers together and divides the answer by 3
  (define (average-plus1 x y)
  (/ (+ x y) 3))
  
  ;;this function checks if the guess is a close enough approximation
  (define (good-enough? guess)
    (< (abs (- (* guess guess guess) x)) 0.001))
  
  ;;this function improves the guess
  (define (improve guess)
    (average-plus1 (/ x (* guess guess)) (* 2 guess)))
  
  ;;iterative cube root function
  (define (cbrt-iteration guess)
    (if (good-enough? guess) ;base case
        guess
        (cbrt-iteration (improve guess)))) ;tail recursion
  
  (cbrt-iteration 1.0))

;;Testing
(my-cbrt 8)       ;;Expected Output: Close to 2
(my-cbrt 125)     ;;Expected Output: Close to 5  
(my-cbrt 512)     ;;Expected Output: Close to 8  
(my-cbrt 1024)    ;;Expected Output: Close to 10.07



;;; -----------------------------------------------------------------------------------------------------


;Question 9  
(newline)
(display "Question 9")
(newline)
; The behaviour for the procedure a-b is:
; if the variable b is greater than 0 the operation that will occur is going to be a + b
; if the variable b is equal to 0, the operation that will occur is going to be a - b
; Otherwise (remaining case is if b is negative), the operation is going to a * b

(define (a-b a b)
  ((cond ((> b 0) +) ((= b 0) -) (else *)) a b))

; case where b is greater than 0
(a-b 1 3)   ;; Expected Output: 4

; case where b is equal to 0
(a-b 1 0)   ;; Expected Output: 1

; case where b lower than 0
(a-b 5 -5)  ;; Expected Output: 25


;;; -----------------------------------------------------------------------------------------------------


;Question 10  
(newline)
(display "Question 10")
(newline)


;new-if function
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;my-sqrt function
(define (my-sqrt x)
  
  ;;this function finds the average of 2 numbers
  (define (average x y)
  (/ (+ x y) 2))
  
  ;good enough function
  (define (good-enough? guess)
    (< (abs (- (* guess guess) x)) 0.001))
  
  ;;this function improves the guess
  (define (improve guess)
    (average guess (/ x guess)))
  
  ;modifited sqrt-iteration function where close-enough? function is passed in 
  ;as a parameter and the maximum number of  iterations is passed also passed as a parameter
  (define (sqrt-iteration guess close-enough? count max-iter)
    (if (or (close-enough? guess) (= count max-iter))
        guess
        (sqrt-iteration (improve guess) close-enough? (+ count 1) max-iter)))
  
  (sqrt-iteration 1.0 good-enough? 0 6))

;test cases
(my-sqrt 0)    ;Expected Output: Close to 0
(my-sqrt 2)    ;Expected Output: Close to 1.41
(my-sqrt 4)    ;Expected Output: Close to 2
(my-sqrt 32)   ;Expected Output: Close to 5.65
(my-sqrt 25)   ;Expected Output: Close to 5
(my-sqrt 512)  ;Expected Output: Close to 22.62
(my-sqrt 16)   ;Expected Output: Close to 4

;lower precision because of only 6 iterations
(my-sqrt 1024) ;Expected Output: Close to 32

;c) new-if does not work because new-if is a function which means all arguments are evaluated before entering 
;   the function and sqrt-iteration is a recursive function that needs to stop recurring when 
;   good-enough returns true.



