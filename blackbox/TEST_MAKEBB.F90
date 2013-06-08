!  Test_MakeBB.f90 
!
!  FUNCTIONS:
!	Test_MakeBB      - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: Test_MakeBB
!
!  PURPOSE:  Entry point for the console application.
!
!  AUTHOR: Stuart Campbell
!
!****************************************************************************

	program Test_MakeBB

	implicit none

	! Variables

	integer :: i
	real	:: inten(2000), wav(2000), tim(2000) 
	real	:: time_start, time_step, wavelength
	real	:: intensity

	! Body of Test_MakeBB
	
	wavelength = (4.01 + 4.04) / 2.0	! Angstroms
	time_start= 0.0	! Starting value
	time_step = 5.0d-6


	do i = 1, 2000
		wav(i) = wavelength
		tim(i) = time_start + (i-1)*time_step
	enddo

!	call TS2_COUPLED_GROOVED_v1(2000, wav, tim, inten)
	call TS2_DECOUPLED_SHARP_v1_1(2000, wav, tim, inten)

	open(unit=10,file="test_makebb.dat",status='new')
	do i = 1, 2000	
		write(10,*) tim(i), inten(i)	
	enddo
	close(10)

	end program Test_MakeBB

