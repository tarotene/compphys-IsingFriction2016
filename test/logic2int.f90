PROGRAM main

  IMPLICIT NONE

  LOGICAL(8) :: l1 = .TRUE., l2 = .FALSE.

  l1 = LOGICAL(2 < 1)
  l2 = LOGICAL(2 > 0)
  WRITE(0, '(a, l2, a, l2)') "l1 = ", l1, ", l2 = ", l2
	WRITE(0, '(a, i2, a, i2)') "l1 = ", -l1*1, ", l2 = ", -l2*1

END PROGRAM main
