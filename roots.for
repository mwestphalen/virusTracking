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
        INTEGER :: polyOrder
        INTEGER :: aTerm, bTerm, cTerm, dTerm = 0
        INTEGER :: tolerance

* Prompt
        WRITE(*,*) 'With the help of the false-position method, this'
        WRITE(*,*) 'program attempts to find the roots of a polynomial'
        WRITE(*,*) 'equation (less than order 4) given its terms and'
        WRITE(*,*) 'the tolerance within which to stop iterating, with'
        WRITE(*,*) '-5 <= x <= 5.'
        WRITE(*,*)

* Getting polynomial order
1     WRITE(*,11) 
11    FORMAT('Order of Polynomial (less than 4): ', $)
      READ(*,*, iostat=ierror) polyOrder
      IF (ierror .NE. 0 .OR. polyOrder > 4 .OR. polyOrder < 0) THEN
        WRITE(*,*) 'Error: Wrong input type. Try again.'
        WRITE(*,*)
        GO TO 1
      ENDIF
      WRITE(*,*)

* Getting polynomial terms
      CALL setTerm(aTerm)
      CALL setTerm(bTerm)
      CALL setTerm(cTerm)
      IF (polyOrder .EQ. 4) THEN
        CALL setTerm(dTerm)
      ENDIF
      
* Getting the tolerance to stop iterating
3     WRITE(*,13)
13    FORMAT('Enter the tolerance: ', $)
      READ(*,*, iostat=ierror) tolerance
      IF (ierror .NE. 0) THEN
            WRITE(*,*) 'Error: Wrong input type. Try again.'
            write(*,*)
            GO TO 3
      ENDIF

      

      STOP
      END

****************
* This soubroutine gets the user input for each term in a polynomial
      SUBROUTINE setTerm(term)
        INTEGER :: term;
2       WRITE(*,12)
12      FORMAT('Enter term : ', $)
        READ(*,*, iostat=ierror) term
        IF (ierror .NE. 0) THEN
            WRITE(*,*) 'Error: Wrong input type. Try again.'
            WRITE(*,*)
            GO TO 2
        ENDIF
      WRITE(*,*)
      END