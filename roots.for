*************************
* This program finds the roots of a polynomial equation using the
* false-position method. 
* 
* The user inputs the terms for the polynomial
* and the tolearnce within which to stop iterating. Polynomials may
* not be of an order higher than 4, and -5 <= x <= 5.

* Labels:
* 1-9: Input Check
* 11-19: Output formatting
*
* Written by: M. Westphalen
* 10/06/2022
*************************

      PROGRAM roots

* Variable Declaration/Initialization
        INTEGER :: numOfTerms
        REAL :: tolerance, absError = 100
        REAL :: a = -5
        REAL :: b = 5
        REAL :: c = 0
        REAL :: cBefore
        REAL :: f_of_a, f_of_b, f_of_c
        REAL :: polynomial
        DIMENSION polynomial(8)
        DATA polynomial/0,0,0,0,0,0,0,0/

* Prompt
        WRITE(*,*)  
        WRITE(*,*) 'With the help of the false-position method, this'
        WRITE(*,*) 'program attempts to find the roots of a polynomial'
        WRITE(*,*) 'equation (less than order 4) given its terms and'
        WRITE(*,*) 'the tolerance within which to stop iterating, with'
        WRITE(*,*) '-5 <= x <= 5.'
        WRITE(*,*)

* Getting polynomial order
1     WRITE(*,11) 
11    FORMAT('Number of terms (less than 4): ', $)
      READ(*,*, iostat=ierror) numOfTerms
      IF (ierror .NE. 0 .OR. numOfTerms > 4 .OR. numOfTerms < 0) THEN
        WRITE(*,*) 'Error: Wrong input type. Try again.'
        WRITE(*,*)
        GO TO 1
      ENDIF
      WRITE(*,*)

* Getting polynomial terms
* Every 2 digits in the array represent the term and its power
* as gathered fro the user (ex: pol(1)=4,pol(2)=2 => 4x^2)
      CALL setTerm(polynomial(1), polynomial(2))
      IF (numOfTerms == 2) THEN
            CALL setTerm(polynomial(3), polynomial(4))
      ELSEIF (numOfTerms == 3) THEN
            CALL setTerm(polynomial(3), polynomial(4))
            CALL setTerm(polynomial(5), polynomial(6))
      ELSEIF (numOfTerms == 4) THEN
            CALL setTerm(polynomial(3), polynomial(4))
            CALL setTerm(polynomial(5), polynomial(6))
            CALL setTerm(polynomial(7), polynomial(8))
      ENDIF 
      
* Getting the tolerance to stop iterating
4     WRITE(*,14)
14    FORMAT('Enter the tolerance: ', $)
      READ(*,*, iostat=ierror) tolerance
      IF (ierror .NE. 0) THEN
            WRITE(*,*) 'Error: Wrong input type. Try again.'
            WRITE(*,*)
            GO TO 4
      ENDIF

* Displaying equation to the user before finding the roots
      WRITE(*,*) 
*     TODO

* Staring iterations to find root
      DO WHILE(absError > tolerance)
            cBefore = c
* Solving for c from secant line equation between f(a) and f(b)
            CALL solve(f_of_a,a,polynomial)
            CALL solve(f_of_b,b,polynomial)

            WRITE(*,*) 'f(a) = ',f_of_a
            WRITE(*,*) 'f(b) = ',f_of_b

* If f(a) * f(b) >= 0, then it means there's zero or multiple roots
* present, and the false-position method won't be helpful...
            IF (f_of_a * f_of_b >= 0) THEN
                  WRITE(*,*) 'The polynomial has 0 or multiple roots'
                  WRITE(*,*) 'therefore, the false-position method'
                  WRITE(*,*) 'will not be helpful.'
                  STOP
            c = ((a*f_of_b)-(b*f_of_a)) / (f_of_b - f_of_a)
            WRITE(*,*) 'C is: ', c
            ENDIF

* Defining whether the root is to the left or to the right of c
            CALL solve(f_of_c,c,polynomial)
            IF (f_of_c == 0) THEN
                 WRITE(*,*) 'The root is: ', c
                 STOP
            ELSE IF (f_of_a * f_of_c < 0) THEN
                  b = c
                  absError = ABS((c - cBefore) / c)
            ELSE IF (f_of_b * f_of_c < 0) THEN
                  a = c
                  absError = ABS((c - cBefore) / c)
            ENDIF
      ENDDO

      WRITE(*,*)
      WRITE(*,*)'The root of the given function (within the tolerance)'
      WRITE(*,*) 'is: ', c
      WRITE(*,*) 

      STOP
      END

****************
* This soubroutine gets the user input for each term in a polynomial
      SUBROUTINE setTerm(term, power)
        REAL :: term, power
2       WRITE(*,12)
12      FORMAT('Enter term: ', $)
        READ(*,*, iostat=ierror) term
        IF (ierror .NE. 0) THEN
            WRITE(*,*) 'Error: Wrong input type. Try again.'
            WRITE(*,*)
            GO TO 2
        ENDIF
3       WRITE(*,13)
13      FORMAT('Enter power: ', $)
        READ(*,*,iostat=ierror) power
        IF (ierror .NE. 0 .OR. power > 4 .OR. power < 0) THEN
            WRITE(*,*) 'Error: Wrong input type. Try again.'
            WRITE(*,*)
            GO TO 3
        ENDIF
      WRITE(*,*)
      END

**************
* This subroutine solves for f(x) given x and the terms and
* powers of the polynomial
      SUBROUTINE solve(f_of_x,x,polynomial)
            REAL :: f_of_x, x
            REAL :: polynomial
            DIMENSION polynomial(8)

            f_of_x = polynomial(1)*(x**polynomial(2))
            f_of_x = f_of_x + polynomial(3)*(x**polynomial(4))
            f_of_x = f_of_x + polynomial(5)*(x**polynomial(6))
            f_of_x = f_of_x + polynomial(7)*(x**polynomial(8))
      END