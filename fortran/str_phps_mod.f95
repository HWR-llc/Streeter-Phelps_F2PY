MODULE str_phps_mod
IMPLICIT NONE

CONTAINS
! SUBROUTINES
SUBROUTINE run(r_len, r_wid, r_flow, r_s, man_n, elem_count, ww_flow, r_t0, bod_k, rbod0, rdo0, wwbod0, wwdo0, r_x, r_dos, r_dod)
  REAL, INTENT(IN) :: r_len, r_wid, r_flow, r_s, man_n, ww_flow, r_t0, bod_k, rbod0, rdo0, wwbod0, wwdo0
  ! river length (m), river width (m), river flow (m^3/s), river slope (m/m), Manning's n (unitless), WWTP flow (m^3/s), river temperature (C), 
  ! bod decay constant (1/days), initial bod in river (mg/L), initial river do (mg/L), initial WWTP effluent bod (mg/L), initial WWTP effluent do (mg/L)
  INTEGER, INTENT(IN) :: elem_count ! number of locations to calculate DO, evenly spaced along r_len
  ! dim river inputs
  REAL, INTENT(OUT), DIMENSION(elem_count) :: r_x ! longitudinal positions along river (m)
  REAL, DIMENSION(elem_count) :: r_u ! river velocity (m/s) (array)
  REAL, DIMENSION(elem_count) :: r_h ! river depth (m) (array)
  REAL, DIMENSION(elem_count) :: r_t ! river temp (C) (array)
  ! dim upstream conditions
  REAL flow_0, bod_0, do_0 ! cumulative flow of river and WWTP (m^3/s), initial bod (mg/L), initial do (mg/L)
  ! results array
  REAL, INTENT(OUT), DIMENSION(elem_count) :: r_dos ! longitudinal do saturation at each x (mg/L) (array)
  REAL, INTENT(OUT), DIMENSION(elem_count) :: r_dod ! longitudinal do deficit at each x (mg/L) (array)
  REAL, DIMENSION(elem_count) :: r_do ! longitudinal do at each x (mg/L)
  ! calculation variables
  INTEGER i, j
  REAL x_x_u, avg_u, time, do_sat, do_sat0, do_def, sec_2_day
  ! distance multiplied by velocity, longitudinal average velocity (m/s), elapsed time at location (seconds), do saturation at location (mg/L), do saturation at x=0 (mg/L), constant 
  ! constants
  sec_2_day = 86400

  ! calc cumulative flow and flow-weighted values
  flow_0 = r_flow + ww_flow
  bod_0 = (r_flow * rbod0 + ww_flow * wwbod0) / flow_0
  do_0 = (r_flow * rdo0 + ww_flow * wwdo0) / flow_0
  ! river geometry arrays
  CALL init_geo(r_len, r_wid, flow_0, r_s, man_n, elem_count, r_x, r_u, r_h, r_t, r_t0)
  ! initial DO sat
  do_sat0 = do_sat_calc(r_t0)
  ! populate results arrays
  PRINT *, "    Dist (m)", "       Time (d)","        DO Sat (mg/L)", "   DO Def (mg/L)"
  DO i = 1, elem_count
    ! calculate average velocity to current point
    DO j = 1, i
      IF (j == 1) THEN
        x_x_u = 0
      ELSE
        x_x_u = x_x_u + (r_x(j) - r_x(j-1)) * ((r_u(j) + r_u(j-1)) / 2)
      END IF
    END DO
    IF (i == 1) THEN
      avg_u = 0
    ELSE
      avg_u = x_x_u / r_x(i)
    END IF
    IF (i == 1) THEN
      time = 0
    ELSE
      time = (r_x(i) / avg_u) * (1/sec_2_day) !conversion to days from seconds
    END IF
    CALL calc_step(time, r_u(i), r_h(i), r_t(i), bod_k, bod_0, do_0, do_sat0, do_sat, do_def)
    PRINT *, r_x(i), time, do_sat, do_def
    r_dos(i) = do_sat
    r_dod(i) = do_def
    r_do(i) = do_sat - do_def
  END DO
  CALL save_result(r_x, r_dos, r_do)
END SUBROUTINE run

SUBROUTINE read_inp(file_path, r_len, r_wid, r_flow, r_s, man_n, elem_count,ww_flow, r_t0, bod_k, rbod0, rdo0, wwbod0, wwdo0)
  CHARACTER(len=100), INTENT(IN) :: file_path
  REAL, INTENT(OUT) :: r_len, r_wid, r_flow, r_s, man_n, ww_flow, r_t0, bod_k, rbod0, rdo0, wwbod0, wwdo0
  INTEGER, INTENT(OUT) :: elem_count
  CHARACTER(len=100) line
  INTEGER inp_total, inp_count
  LOGICAL colon
  INTEGER char_pos
  CHARACTER(len=10) var_name
  ! CHARACTER(len=100) var_value
  REAL var_value
  999 FORMAT(F16.0)
  char_pos = 1
  inp_total = 11
  inp_count = 1
  OPEN(unit = 300, file = file_path)
  DO WHILE(inp_count <= inp_total)
    READ(300, '(a)') line
    colon = .TRUE.
    IF (line(:1) /= "!") THEN
      char_pos = 1
      DO WHILE (colon)
        IF (line(char_pos: char_pos + 1) == ":") THEN
          colon = .FALSE.
          var_name = line(1:char_pos - 1)
          READ(line(char_pos + 2: 100),999) var_value
          SELECT CASE (var_name)
            CASE("rlen")
              r_len = var_value
            CASE("rwid")
              r_wid = var_value
            CASE("rq")
              r_flow = var_value
            CASE("rs")
              r_s = var_value
            CASE("n")
              man_n = var_value              
            CASE("wwq")
              ww_flow = var_value
            CASE("rint")
              elem_count = var_value
            CASE("rtmp")
              r_t0 = var_value
            CASE("bod_k")
              bod_k = var_value
            CASE("rbod0")
              rbod0 = var_value
            CASE("rdo0")
              rdo0 = var_value 
            CASE("wwbod0")
              wwbod0 = var_value 
            CASE("wwdo0")
              wwdo0 = var_value 
            CASE DEFAULT
              PRINT *,"unrecognized model input in .inp file"
          END SELECT
          inp_count = inp_count + 1
        ELSE
          char_pos = char_pos + 1
        END IF
      END DO
    END IF     
  END DO
  CLOSE(300)
END SUBROUTINE read_inp

SUBROUTINE init_geo(r_len, r_wid, flow, r_s, man_n, elem_count, r_x, r_u, r_h,r_t, r_t0)
  ! river length in meters, river width in meters, cumulative flow (m^3/s), longitudinal river position (m) (array), river velocity at x (m/s) (array), river depth at x (m) (array),
  ! river temp at x (C) (array)
  REAL, INTENT(IN) :: r_len, r_wid, flow, r_s, man_n, r_t0
  INTEGER, INTENT(IN) :: elem_count
  REAL, INTENT(OUT) :: r_x(:)
  REAL, INTENT(OUT) :: r_u(:)
  REAL, INTENT(OUT) :: r_h(:)
  REAL, INTENT(OUT) :: r_t(:)
  INTEGER i
  DO i = 1, elem_count
    r_x(i) = (r_len / elem_count) * (i - 1)
    r_u(i) = mannings_iter(man_n, r_s, r_wid, flow)
    r_h(i) = flow / (r_u(i) * r_wid)
    r_t(i) = r_t0
  END DO
END SUBROUTINE init_geo

SUBROUTINE calc_step(time, u, h, t, bod_k, bod_0, do_0, do_sat0, do_sat, do_def)
  ! time, current velocity (m/s), water depth (m), temperature (C), bod decay rate (1/d), initial bod (mg/L), initial do (mg/L), initial do saturation (mg/L), do saturation (mg/L), do deficit (mg/L)
  REAL, INTENT(IN) :: time, u, h, t, bod_k, bod_0, do_0, do_sat0
  REAL, INTENT(OUT) :: do_sat, do_def
  REAL diffu, reaer

  do_sat = do_sat_calc(t)
  diffu = diff_calc(t)
  reaer = reaer_calc(u, h, t, diffu)

  do_def = (bod_k * bod_0 / (reaer - bod_k)) * (EXP(-bod_k * time) - EXP(-reaer * time)) &
  + (do_sat0 - do_0) * EXP(-reaer * time)
END SUBROUTINE calc_step 
SUBROUTINE save_result(r_x, r_dos, r_do)
  REAL, INTENT(IN) :: r_x(:)
  REAL, INTENT(IN) :: r_dos(:)
  REAL, INTENT(IN) :: r_do(:)
  INTEGER i
  101 FORMAT(f10.2, f10.2, f10.2)
  OPEN(unit = 200, file="DO_curve.txt")
  WRITE(200,*)"dist (m)  ","DO_sat (mg/l)  ","DO (mg/L)  "
  DO i = 1, SIZE(r_x)
    WRITE (200,101) r_x(i), r_dos(i), r_do(i)    
  END DO
  CLOSE(200)
END SUBROUTINE save_result

! Functions
FUNCTION mannings_iter(man_n, r_s, r_wid, in_q)
  ! mannings n (unitless), river slope (m/m), river width (m), river flow (m^3/s)
  REAL mannings_iter, man_n, r_s, r_wid, in_q
  REAL area, cur_d, prev_d, calc_q, cur_dq, prev_dq, tmp_v
  ! cross-sectional flow area (m^2/s), current depth guess (m), previous depth guess (m), current calculated flow (m^3/s), delta between observed and calc'ed flow (m^3/s), 
  ! previous delta between observed and calc'ed flow (m^3/s), changing vel value (m/s)
  LOGICAL guess, bounded
  INTEGER cur_dq_sn, prev_dq_sn, new_dq_sn
  REAL new_d, new_dq
  ! logic controllers for finding value
  cur_d = 1 ! initial guess 1 meter
  cur_dq = 100 ! initialized value to begin loop
  guess = .TRUE.
  bounded = .FALSE.
  DO WHILE (ABS(cur_dq) > 0.01)
    tmp_v = mannings_v(man_n, r_s, r_wid, cur_d)
    area = r_wid * cur_d
    calc_q = tmp_v * area
    cur_dq = in_q - calc_q
    IF (guess) THEN
      prev_d = cur_d
      prev_dq = cur_dq
      cur_d = 2 * cur_d
      guess = .FALSE.
    ELSE
      IF (.NOT. bounded) THEN
        IF ((cur_dq > 0) .AND. (prev_dq > 0)) THEN
          prev_d = cur_d
          cur_d = 2 * cur_d
        ELSE IF ((cur_dq < 0) .AND. (prev_dq < 0)) THEN
          prev_d = cur_d
          cur_d = 0.5 * cur_d
        ELSE IF (((cur_dq > 0) .AND. (prev_dq < 0)) .OR. ((cur_dq < 0) .AND. (prev_dq > 0))) THEN
          bounded = .TRUE.
        ELSE IF (cur_dq == 0) THEN
          PRINT *, "cur_dq equals exactly 0"
        END IF
      ELSE
        cur_dq_sn = cur_dq / ABS(cur_dq)
        prev_dq_sn = prev_dq / ABS(prev_dq)
        new_dq = 100 ! initial value for new while loop
        DO WHILE (ABS(new_dq) > 0.01)
          new_d = (cur_d + prev_d) / 2
          tmp_v = mannings_v(man_n, r_s, r_wid, new_d)
          area = r_wid * new_d
          calc_q = tmp_v * area
          new_dq = in_q - calc_q
          new_dq_sn = new_dq / ABS(new_dq)
          IF (new_dq_sn == cur_dq_sn) THEN
            cur_d = new_d
          ELSE
            prev_d = new_d
          END IF
        END DO
        cur_dq = new_dq
      END IF
    END IF
  END DO
  mannings_iter = tmp_v
END FUNCTION mannings_iter
FUNCTION mannings_v(man_n, r_s, r_wid, r_dep)
  ! mannings n (unitless), river slope (m/m), river width (m), river_depth (m)
  ! assume rectangular channel
  REAL mannings_v, man_n, r_s, r_wid, r_dep
  REAL area, peri, hy_rad
  ! area (m^2), perimeter (m), hydraulic radius (m)
  area = r_wid * r_dep
  peri = r_dep * 2 + r_wid ! assume rectangular channel
  hy_rad = area / peri
  mannings_v = (1 / man_n) * (hy_rad) ** (REAL(2) / REAL(3)) * (r_s ** (REAL(1) / REAL(2)))
END FUNCTION mannings_v 
FUNCTION do_sat_calc(tx)
  ! temperature (C)
! QUAL2K DO sat calc
  REAL do_sat_calc, tx, tk, lno
  tk = tx + 273.15
  lno = -139.34411 + (1.575701E5 / tk) - (6.642308E7/tk**2) + (1.243800E10/tk**3) - (8.621949E11/tk**4)
  do_sat_calc =  EXP(lno) 
END FUNCTION do_sat_calc
FUNCTION reaer_calc(ux,hx,tx,diffx)
  ! velocity (m/s), depth (m), temperature (C), diffusion coeffecient for air to water (m^2/s)
  REAL reaer_calc, ux, hx, tx, diffx, k20, sec_2_day
  sec_2_day = 86400
  k20 = ((diffx * ux)**0.5) / (hx**1.5)
  k20 = k20 * sec_2_day !convert to 1/d from 1/s
  reaer_calc = k20*1.047**(tx-20)
END FUNCTION reaer_calc
FUNCTION diff_calc(tx)
  ! temperature (C), returns constant value
  REAL diff_calc, tx
  diff_calc=2.09E-9 ! assumed constant for test in m^2/s
END FUNCTION diff_calc

END MODULE str_phps_mod