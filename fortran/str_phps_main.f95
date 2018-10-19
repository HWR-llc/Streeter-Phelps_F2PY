PROGRAM str_phps_main
USE str_phps_mod
IMPLICIT NONE


! dim user variables
REAL rlen, rwid, rq, rs, man_n, wwq, rtmp, bod_k, rbod0, rdo0, wwbod0, wwdo0
INTEGER rint
REAL, DIMENSION(:), ALLOCATABLE :: riv_x
REAL, DIMENSION(:), ALLOCATABLE :: riv_dos
REAL, DIMENSION(:), ALLOCATABLE :: riv_dod
! ! user inputs
! rlen    --> length (m)
! rwid    --> width (m)
! rq      --> river flow (cub. m/s)
! rs      --> river slope (m/m)
! wwq     --> wastewater flow (cub. m/s)
! rint    --> number of elements in model
! rtmp    --> river temp (C) (constant)
! bod_k   --> BOD decay rate (1/d)
! rbod0   --> BOD load in river (mg/L)
! rdo0    --> DO in river (mg/L)
! wwbod0  --> BOD load from WWTP (mg/L)
! wwdo0   --> DO in WWTP eff (mg/L)
! ----------------------------------------------
! riv_x   --> longitudinal position along river (array)
! riv_dos --> do saturation at each x position (array)
! riv_dod --> do deficit at each x position (array)

! file name for reading from text file
CHARACTER(len=100) file_name
file_name = "str_phps.inp"

! main code
PRINT *
PRINT *,"*********************************"
PRINT *,"Streeter-Phelps Solution"
PRINT *,"*********************************"

CALL read_inp(file_name, rlen, rwid, rq, rs, man_n, rint, wwq, rtmp, bod_k, rbod0, rdo0, wwbod0, wwdo0)
PRINT *,"---loading input file complete complete---"
ALLOCATE(riv_x(rint))
ALLOCATE(riv_dos(rint))
ALLOCATE(riv_dod(rint))
CALL run(rlen, rwid, rq, rs, man_n, rint, wwq, rtmp, bod_k, rbod0, rdo0, wwbod0, wwdo0, riv_x, riv_dos, riv_dod)
PRINT *,"-------model run complete--------"

END PROGRAM str_phps_main