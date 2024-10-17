  !> @brief Functions to sleep or pause
  !>
  !> These wrap C functions so that end user doesn't
  !> have to deal with C interoperability
  !> or finding the 'correct' way to invoke sleep
  !> @author CS Brady


MODULE sleep_mod

  ! Uses standard C interoperability stuff
  USE ISO_C_BINDING
  IMPLICIT NONE

  !> Specifies a time value
  TYPE, BIND(C) :: timespec
    INTEGER(C_LONG) :: tv_sec
    INTEGER(C_LONG) :: tv_nsec
  END TYPE timespec


  INTERFACE
    !> Wrapper around C function sleeping for given number of seconds
    !>@param seconds Number of seconds to sleep for
    !>@return The time remaining (seconds-seconds_actually_slept)
    FUNCTION csleep(seconds) RESULT(remaining) BIND(C, NAME='sleep')
      IMPORT C_INT
      INTEGER(C_INT), VALUE :: seconds
      INTEGER(C_INT) :: remaining
    END FUNCTION csleep

    !> Wrapper around C function sleeping for given number of nanoseconds
    !>@param req Number of seconds requested to sleep for
    !>@param rem Number of seconds actually slept for
    !>@return 0 if the correct time was slept for, -1 else
    FUNCTION nanosleep(req, rem) BIND(C, NAME='nanosleep')
      IMPORT C_INT, timespec
      TYPE(timespec) :: req, rem
      INTEGER(C_INT) :: nanosleep
    END FUNCTION nanosleep

  END INTERFACE



  CONTAINS

  !> Wrapper around the sleep function to swallow the return value
  !>@param seconds Number of seconds to sleep for

  SUBROUTINE sleep_sec(seconds)
    INTEGER(C_INT) :: res
    INTEGER(KIND=SELECTED_INT_KIND(9)), INTENT(IN) :: seconds
    res = csleep(seconds)

  END SUBROUTINE sleep_sec

  !> Sleep for a number of milliseconds
  !> For the purposes here, this is the 'sane' shortest time period to use
  !>@param milliseconds Number of milliseconds to sleep for
  SUBROUTINE millisleep(milliseconds)
    INTEGER, INTENT(IN) :: milliseconds
    TYPE(timespec) :: spec_in, spec_out
    INTEGER(C_INT) :: res

    spec_in%tv_sec = milliseconds / 1000
    spec_in%tv_nsec = MOD(milliseconds, 1000) * 1000000
    res = nanosleep(spec_in, spec_out)

  END SUBROUTINE millisleep


  SUBROUTINE busy_sleep(milliseconds)

    INTEGER, INTENT(IN) :: milliseconds
    INTEGER(KIND=SELECTED_INT_KIND(15)) :: i, iter
    REAL(KIND=SELECTED_REAL_KIND(15, 307)) :: clock, tmp

    INTEGER :: cnt, cnt_rate, poll, end_ticks, last

    ! Get current time
    CALL SYSTEM_CLOCK(cnt, cnt_rate)

    ! Work out end time - units is now milliseconds
    end_ticks = FLOOR(cnt + milliseconds * 1000.0/REAL(cnt_rate))

    ! Tune polling interval
    poll = 1d2
    last = 0

    OPEN(file='/dev/null', status='old', unit=101, action='write')
    ! Do a busy-wait loop, checking the clock periodically
    ! Writing to a file takes a bit longer than a simple numeric op and 
    ! makes sure this can't be optimised away
    DO WHILE(cnt < end_ticks)
      tmp = tmp + 1.0
      WRITE(101, *) tmp
      IF(last > poll) THEN
        CALL SYSTEM_CLOCK(cnt)
        last = 0
      END IF
      last = last + 1
    END DO

    CLOSE(101)

  END SUBROUTINE busy_sleep

END MODULE sleep_mod
