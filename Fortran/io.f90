MODULE IO

  USE sleep_mod
  USE command_line
  USE random_mod
  IMPLICIT NONE

  CONTAINS
  
  SUBROUTINE painfully_slow_intro_message(filename, delay)

    CHARACTER(LEN=*), INTENT(IN) :: filename
    CHARACTER :: next
    INTEGER :: lun, stat
    INTEGER, INTENT(IN), OPTIONAL :: delay
    INTEGER :: delay_in

    ! Set Default delay
    IF(PRESENT(delay)) THEN
      delay_in = delay
    ELSE
      delay_in = 10
    END IF

    !Open file
    OPEN(newunit=lun, file=filename, action='read')

    next = " "
    DO WHILE(.TRUE.) !Loop until an explicit exit
      !Print one char at a time with a sleep
      !Read one character...
      READ(lun, '(A)', IOSTAT=stat, ADVANCE='NO') next
      !Check for EndOfFile
      IF (IS_IOSTAT_END(stat)) THEN
        EXIT
      ENDIF
      !Write it, unless it's a newline
      IF(IS_IOSTAT_EOR(stat)) THEN
        WRITE(*, '(A)') ''
      ELSE
        WRITE(*, '(A)', ADVANCE='NO') next
      END IF
      ! Sleep. This mimics cases where our code is blocked by a system call
      CALL millisleep(delay_in)
    END DO

  END SUBROUTINE

  FUNCTION get_selection(maxv) RESULT(sel)
    INTEGER :: sel, sel_tmp
    INTEGER, INTENT(IN) :: maxv !Top limit for cases
    LOGICAL :: success, exists

    CALL parse_args

    success = get_arg("run", sel_tmp, exists=exists)
    IF(success .AND. exists .AND. sel_tmp .GT. 0 .AND. sel_tmp .LE. maxv) THEN
      sel = sel_tmp
    ELSE
      sel = 1 + FLOOR(random() * maxv)
    END IF
  END FUNCTION


END MODULE
