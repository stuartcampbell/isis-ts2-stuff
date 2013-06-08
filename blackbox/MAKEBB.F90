!
! Stuart Campbell (19th July 2001)
!

! FAMILY (STRING) Defines what 'family' the routines belong to...
!	...for example we might have the following generated routines:
!
!			FUNCTION ISIS_CH4_V1
!			FUNCTION ISIS_H2_V1
!			FUNCTION ISIS_H20_V1
!   ... Then the family will be "_V1"


	PROGRAM MakeBB

	CHARACTER	:: filename*80, procname*80, version*5
	INTEGER		:: tally, surface, tstation
	REAL		:: area

!	filename = "ts326.o"	! TS I
!	filename = "ts322.o"	! TS II (v1.1)

! Better energy bins...
!	filename = "ts331.o"	! TS II (v1.1)

	OPEN(UNIT=20, FILE='makebb_ts333.inp', STATUS='OLD')
!	OPEN(UNIT=20, FILE='makebb_ts332.inp', STATUS='OLD')

!	READ(20,100,END=50) filename, tally, surface, area, procname, version
	DO i = 1, 50
	READ(20,*,END=50) filename, tally, surface, area, procname, version, tstation
	
	WRITE(6,*) 'Filename :', trim(filename)
	WRITE(6,*) 'Tally    :', tally
	WRITE(6,*) 'Surface  :', surface
	WRITE(6,*) 'Area     :', area
	WRITE(6,*) 'Procname :', trim(procname)
	WRITE(6,*) 'Version  :', trim(version)
	WRITE(6,*) 'Target   :', tstation

	Call Make_Template(filename, tally, surface, area, procname, version, tstation)
	
	ENDDO

50	CLOSE(UNIT=20)

100	FORMAT(A,I,I,F,A,A)

	END PROGRAM MakeBB
