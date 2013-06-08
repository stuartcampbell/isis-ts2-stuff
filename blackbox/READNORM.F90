!  ReadNorm.f90 
!
!  FUNCTIONS:
!	ReadNorm      - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: ReadNorm
!
!  PURPOSE:  Read (and norm) in simulation data from MCNP-X output file.
!
!****************************************************************************

	PROGRAM ReadNorm

! Variables
	CHARACTER	:: infile*80, outfile*80
	INTEGER		:: i, j, tally, cos_bin
	REAL		:: area, npratio, Sr, Hz, lmin, lmax
	REAL		:: tm(400),tm_tally(400,9),tm_err(400,9), lam(30)
	REAL		:: en(100), en_tally(100), en_err(100), dele(2,9)
	REAL		:: dataArr(400,9), ebins(9), tbins(400)

! Body of ReadNorm
	
! Read input data filename...
	WRITE(6,*) ' Enter the name of the data file :: '
	READ(5,'(A)') infile
	infile = 'ts326.o'
!	infile = 'c:\temp\ts326.o'
	WRITE(6,*) ' Enter the name of the output file :: '
	READ(5,'(A)') outfile
	!outfile = 'c:\temp\ts326-11-1.out'
!	infile = trim(infile)


!	WRITE(6,*) ''
	WRITE(6,100) trim(infile)

! Get the Tally number...
	WRITE (6,*) 'Enter the tally number :: '
	READ (5,*) tally


! Get the cosine bin to read...
30	WRITE (6,*) 'First or last cosine bin (1/2) :: '
	READ (5,*) cos_bin
	cos_bin = 1
	IF (cos_bin .NE. 1 .AND. cos_bin .NE. 2) THEN 
		WRITE (6,'(1X,A)') 'ERROR 1 or 2 only'
		GOTO 30
	END IF

! Get the area...
	WRITE (6,*) 'Enter area (cm2) :: '
	READ (5,*) area
	area = 144.0

! Open the data file...
	OPEN(unit=10, file=infile, status='OLD')
	OPEN(unit=11, file=outfile, status='NEW')

	CALL readtime(10,tally,cos_bin,tm,tm_tally,tm_err,dele)
!	CALL readenergy(11,tally,cos,en,en_tally,en_err)	
		
	npratio = 3.75d14									! n/p ratio
	Sr = 2.0d0 * 3.1415926d0 * (1.0d0 - 0.87d0)			! Solid angle
	Hz = 10.d0											! frequency

	DO is = 1,9

		!nd(is+1) = 390
		lmin = SQRT(81.81d0/dele(2,is))
		lmax = SQRT(81.81d0/dele(1,is))
		lam(is) = (lmax + lmin) / 2.0d0
		scale = 1.0d0 / (lmax - lmin)

		ebins(i) = (lmax - lmin) / 2.0

!		xcap(is+1) = 'Time'
!		xlab(is+1) = '[us]'
!		ycap(is+1) = 'Flux'
!		ylab(is+1) = 'n/cm2/s/Sr/Ang'
!		WRITE (title(is),'(1X,A,F5.2,A,F5.2,A)') 'Pulse shape between ',lmin,'Ang and ',lmax,'Ang'

		DO i = 2,391		!removes first bin (error in MCNPX input)
			tbins(i) = (tm(i-1) + tm(i)) / 2.0d0
			dataArr(i,is) = tm_tally(i,is) * scale * npratio / Sr / area / Hz / (tm(i) - tm(i-1))
!			ysig(i,is) = tm_err(i,is) * y(i,is)
		ENDDO
	ENDDO

	WRITE(11,*) 'Energy bins...'
	WRITE(11,*)	(ebins(j), j=1,9)
	WRITE(11,*) '...and in wavelength'
	WRITE(11,*) (lam(j), j=1,9)
	DO i = 1, 390
		WRITE(11,*) tbins(i), (dataArr(i,j),j = 1, 9)
	ENDDO


! Close the inut data file...
	CLOSE(unit=10)
	CLOSE(unit=11)

100	FORMAT(/1X,'Using ',A,' as input file'/)

	END PROGRAM ReadNorm

