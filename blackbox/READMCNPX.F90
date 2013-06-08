SUBROUTINE readtime (iunit,tally,cos,x,y,e,dele)
implicit none
!
! A simple program to find and read time tallies
! will have to be re-written for each case.
!

INCLUDE "BB.INC"

!INTEGER NX, NY
!PARAMETER(NX=9,NY=390)
REAL x(NY+1),y(NY+1,NX),e(NY+1,NX), dele(2,NX), t(4), te(4)
REAL dummy
CHARACTER line*256,tag*10
INTEGER iunit, tally, cos, i, is, k

WRITE (tag,'(A7,I3)') '1tally ',tally

10  READ (iunit,'(A)') line
IF (line(1:10) .NE. tag) GOTO 10

20 READ (iunit, '(A)') line   !miss the first energy bin
IF (line(1:12) .NE. ' energy bin:') GOTO 20

IF (cos .EQ. 1) THEN
   DO is=1,NX
30    READ (iunit, '(A)') line
      IF (line(1:12) .NE. ' energy bin:') GOTO 30
      READ (line,'(T14,G13.5,T30,G13.5)') dele(1,is),dele(2,is)
	  dele(1,is) = dele(1,is) * 1.0d9 
	  dele(2,is) = dele(2,is) * 1.0d9    
	  print *, "Emin/Emax = (in ReadMCNPX) ", dele(1,is), dele(2,is)
	  READ (iunit,'(A)') line ; READ (iunit,'(A)') line
      DO i = 1,NY+1
!		 print *, i, x(i)
         READ (iunit,*) x(i),(t(k),te(k),k=1,4)
		 x(i) = x(i) / 1.0d8
		 y(i,is) = t(1) 
		 e(i,is) = te(1)
      END DO
   END DO
ELSE
!   DO i = 1,397
!   DO i = 1,NY+6
!      READ (iunit,'(A)') line
!   END DO
   DO is = 1,NX
40    READ (iunit, '(A)') line
      IF (line(1:12) .NE. ' energy bin:') GOTO 40
      READ (line,'(T14,F13.5,T30,F13.5)') dele(1,is),dele(2,is)
	  dele(1,is) = dele(1,is) * 1.0d9 
	  dele(2,is) = dele(2,is) * 1.0d9
  	  print *, "Emin/Emax = (in ReadMCNPX) ", dele(1,is), dele(2,is)
	  READ (iunit,'(A)') line ; READ (iunit,'(A)') line
      DO i = 1,NY+1
         READ (iunit,*) x(i),(t(k),te(k),k=1,4)
		 x(i) = x(i) / 1.0d8 
		 y(i,is) = t(4) 
		 e(i,is) = te(4)
      END DO
   END DO
END IF

RETURN
END


SUBROUTINE readenergy (iunit,tally,cos,x,y,e)

REAL*8 x(100),y(100),e(100), t(4), te(4)
REAL*8 dummy
CHARACTER line*256,tag*10
INTEGER iunit, tally, cos

WRITE (tag,'(A7,I3)') '1tally ',tally

10  READ (iunit,'(A)') line
IF (line(1:10) .NE. tag) GOTO 10

20  READ (iunit,'(A)') line
IF (line(1:12) .NE. '      energy') GOTO 20

IF (cos .EQ. 1) THEN
   DO i = 1,82
      READ (iunit,*) x(i),(t(k),te(k),k=1,4)
	  x(i) = x(i) * 1.0d9 ; y(i) = t(1) ; e(i) = te(1)
   END DO
ELSE
   DO i = 1,82
      READ (iunit,*) x(i),(t(k),te(k),k=1,4)
	  x(i) = x(i) * 1.0d9 ; y(i) = t(4) ; e(i) = te(4)
   END DO
END IF

RETURN
END
