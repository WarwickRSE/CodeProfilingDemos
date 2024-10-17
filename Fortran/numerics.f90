MODULE numerics

  USE ISO_FORTRAN_ENV
  USE random_mod
  USE sleep_mod

  IMPLICIT NONE

  CONTAINS

  ! Function to finagle the widgets for the thingummybob
  FUNCTION finagle_widgets(A, B)

    REAL(REAL64), INTENT(IN) :: A
    REAL(REAL64) :: finagle_widgets
    INTEGER :: B, i, t
    INTENT(IN) :: B ! Yes, this is valid. Type is just an attribute and we can add them
                    ! in multiple lines. It's a bit mad to do though...

    ! Do Mock work - sleep for A seconds, B times
    t = FLOOR(A*1000.0)
    DO i = 1, B
      CALL busy_sleep(t)
      PRINT*, "CALCULATING", REPEAT('.', i)
    END DO
    finagle_widgets = A

  END FUNCTION

  SUBROUTINE reticulate_splines(A, B)
    REAL(REAL64), INTENT(IN) :: A
    INTEGER, INTENT(IN) :: B
    INTEGER :: i

    ! Like before, do some mock work
    DO i = 1, B
      CALL busy_sleep(FLOOR(A*1000.0))
      PRINT*, "Processing..."
    END DO
  END SUBROUTINE

  !SUBROUTINE


  SUBROUTINE square_the_circle(A, B)
    REAL(REAL64), INTENT(IN) :: A
    INTEGER, INTENT(IN) :: B
    INTEGER :: i
    DO i = 1, B
      CALL repair_fabric_of_reality(A/B/2.0)
      CALL busy_sleep(FLOOR(A/B/2.0*1000.0))
    END DO

  END SUBROUTINE

  SUBROUTINE repair_fabric_of_reality(A)
    REAL(REAL64), INTENT(IN) :: A
    
    ! Do something
    CALL busy_sleep(FLOOR(A*1000.0))

  END SUBROUTINE

END MODULE
