	SUBROUTINE Make_Template(filename, tally, cos_bin, area, procname, version, tstation)
	IMPLICIT NONE

	INCLUDE "BB.INC"

!	INTEGER		:: NX, NY, M
!	PARAMETER (NX=45, NY=390, M=NX*NY)

	INTEGER		:: tally, cos_bin, tstation
	REAL*8		:: area
	CHARACTER	:: procname*80, filename*80, version*5, funcname*80
	
	INTEGER		:: is, i, j, index, ifail
	REAL*8		:: tm(NY+1), tm_tally(NY+1,NX)
	REAL*8		:: tm_err(NY+1,NX), dele(2,NX)
	REAL*8		:: npratio, Sr, Hz, scale, lmin, lmax
	REAL*8		:: dataArr(NY,NX), tbins(NY), ebins(NX), lam(NX)
	REAL*8		:: tempArr(NY), smooth(NY), rough(NY), smoothArr(NY,NX)

	funcname = trim(procname)//'_'//trim(version)

! Read in MCNPX data file and do the normalisation...

	OPEN(UNIT=10, FILE=trim(filename), STATUS='old')

! Call SMB's reading program...

	CALL readtime(10,tally,cos_bin,tm,tm_tally,tm_err,dele)
	
	Sr = 2.0d0 * 3.1415926d0 * (1.0d0 - 0.87d0)			! Solid angle	
	
	SELECT CASE (tstation)
	CASE (1)
		WRITE(6,*) "Target Station 1 Selected"
		Hz = 50.d0				! frequency	for TS1	
		npratio = 1.25d15		! n/p ratio for 200 uamps		
	CASE (2)
		WRITE(6,*) "Target Station 2 Selected"
		npratio = 3.75d14		! n/p ratio for 60 uamps	
		Hz = 10.d0				! frequency	for TS2
	END SELECT

	DO is = 1, NX		! Loop over energy bins
		
		
		write(6,*) "Energy Bin #",is
		print *, "Emin/Emax = ", dele(1,is), dele(2,is)

		lmin = SQRT(81.81d0/dele(2,is))
		lmax = SQRT(81.81d0/dele(1,is))
		lam(is) = (lmax + lmin) / 2.0d0
		scale = 1.0d0 / (lmax - lmin)
		ebins(is) = (lmax - lmin) / 2.0

		DO i = 2,NY+1		!removes first bin (error in MCNPX input) - (Loop over time bins)
			tbins(i-1) = (tm(i-1) + tm(i)) / 2.0d0
			dataArr(i-1,is) = tm_tally(i,is) * scale * npratio / Sr / area / Hz / (tm(i) - tm(i-1))
		ENDDO
	ENDDO	

! Close the MCNPX Data file.
	CLOSE(UNIT=10)
	
! Smooth data...
	DO i = 1, NX	! Loop over energy bins
		tempArr(:) = dataArr(:,i)
		CALL G10CAF(0, NY, tempArr, smooth, rough, ifail)
		smoothArr(:,i) = smooth(:)
	ENDDO

! Open a new file to write the procedure into...

	OPEN(UNIT=80,FILE=trim(funcname)//'.f90',STATUS='new')

! Start function...
	WRITE(80,*) "	SUBROUTINE ", TRIM(funcname), "(npts, wav, tof, inten)"
	WRITE(80,*) "	IMPLICIT NONE"

! Parameters...
	WRITE(80,*) "	INTEGER	:: ifail, nx, ny, n, npts, i"
	WRITE(80,*) "	PARAMETER (nx=",NX,",ny=",NY,",n=nx*ny)"
	WRITE(80,*)	""
	WRITE(80,*)	"	REAL data_x(n), data_y(n), data_z(n)"
	WRITE(80,*)	"	REAL triang(7*n), grads(2,n)"
	WRITE(80,*)	"	REAL wav(npts), tof(npts), inten(npts)"
	WRITE(80,*)	""

! Define data ...
	DO i = 1, NX
		DO j = 1, NY
			index = ((i-1)*NY)+j
!!			WRITE(80,97) index, lam(i)
!!			WRITE(80,98) index, tbins(j)
!!			WRITE(80,99) index, dataArr(j,i)
!			WRITE(80,100) index, lam(i), index, tbins(j), index, dataArr(j,i)
			WRITE(80,100) index, lam(i), index, tbins(j), index, smoothArr(j,i)
		ENDDO
	ENDDO

!	open(unit=20,file='crap.xyz',status='new')
!		DO i = 1, 9
!		tempArr(:) = dataArr(:,i)
!		CALL G10CAF(0, 390, tempArr, smooth, rough, ifail)
!		DO j = 1, 390
!			!WRITE(20,*) lam(i), tbins(j), dataArr(j,i)		! Un-smoothed data
!			WRITE(20,*) lam(i), tbins(j), smoothArr(j,i)	! Smoothed data
!		ENDDO
!	ENDDO
!	close(20)

! do interpolation...
	WRITE(80,*)	""
	WRITE(80,*) "CALL E01SAF(N, data_x, data_y, data_z, triang, grads, ifail)"
	WRITE(80,*)	""
	WRITE(80,*)	"DO i = 1, npts"
	WRITE(80,*) "CALL E01SBF(N, data_x, data_y, data_z, triang, grads, wav(i), &"
	WRITE(80,*) "				tof(i), inten(i), ifail)"
	WRITE(80,*)	"ENDDO"

! Return value...
!	WRITE(80,'(A)') '	'//trim(funcname)//' = inten'
! End function...
	WRITE(80,250) TRIM(funcname)


	CLOSE(unit=80)


97  FORMAT('data_x(', I5, ')=', E15.8)
98  FORMAT('data_y(', I5, ')=', E15.8)
99  FORMAT('data_z(', I5, ')=', E15.8)
100 FORMAT('data_x(',I5,')=',E15.8,' ; data_y(',I5,')=',E15.8,' ; data_z(',I5,')=',E15.8)

250 FORMAT('	END SUBROUTINE ', A)
	
	END SUBROUTINE Make_Template
