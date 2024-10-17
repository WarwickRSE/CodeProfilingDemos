PROGRAM MAIN

  USE ISO_FORTRAN_ENV
  USE IO
  USE numerics

  IMPLICIT NONE


  REAL(REAL64) ::s, t, t2, t3, scal
  INTEGER :: sel, x, x2, x3, y, i, io_del

  !Get selected scenario - if none, then a random one
  sel = get_selection(maxv=4)  
  PRINT('(A, I1)'), "Running setup number ", sel


  !Scale factor on times
  !Set this to tune the total time while keeping the same relatives 
  scal = 1.0
  SELECT CASE(sel)
    CASE(1)
      io_del = 1 ! Delay between prints in init
      t  = 0.1    ! Sleep (~ seconds) in CALCULATING step (finagle_widgets)
      t2 = 1.0    ! Sleep (~ seconds) in PROCESSING step (reticulate_splines)
      t3 = 1.0    ! Sleep (~ seconds) in FINALIZING step (square_the_circle) (TOTAL for all repeats)
      x  = 50     ! Repeats (internal) in CALCULATING step
      x2 = 1      ! Repeats (internal) in PROCESSING step
      x3 = 1     ! Repeats (internal) in FINALIZING step
      y  = 5     ! Repeats (external) of PROCESSING step
    CASE(2)
      io_del = 1
      t  = 0.01
      t2 = 0.1
      t3 = 1.0
      x  = 200
      x2 = 10
      x3 = 20
      y  = 10
    CASE(3)
      io_del = 0
      t  = 0.1
      t2 = 0.01
      t3 = 5.0
      x  = 50
      x2 = 10
      x3 = 10
      y  = 100
    CASE(4)
      io_del = 20
      t  = 0.1
      t2 = 0.01
      t3 = 1.0
      x  = 1
      x2 = 1
      x3 = 1
      y  = 1
  END SELECT

  t  = t  * scal
  t2 = t2 * scal
  t3 = t3 * scal
  io_del = CEILING(io_del * scal) !Defaults to 1 no matter how small we make scal, unless _exactly_ zero

  !--------------------------------------------------------------------------
  !---- here follows the "real" "working" code -----------------------------
  !---- If you look really closely you might notice its actually all dummy
  !---- code, though!


  !Start up code
  CALL painfully_slow_intro_message("../banner.txt", delay=io_del)
  
  ! Doing one thing
  s = finagle_widgets(t, x)

  ! Repeatedly doing another thing
  DO i = 1, y
    PRINT('(A, I5)'), "Wibbling data set: ", i
    CALL reticulate_splines(t2, x2)
  END DO

  !Doing a thrid thing
  PRINT*, "Finalising"
  CALL square_the_circle(t3, x3)

END PROGRAM
