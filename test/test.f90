PROGRAM main
  USE ifport, only: access
  IMPLICIT NONE

  INTEGER(kind = 4) :: stat_acc

  stat_acc = access("test.dat", " ")
  CALL system("echo hogehoge")

END PROGRAM main
