MODULE mod_rand
  USE MKL_VSL_TYPE
  USE MKL_VSL
  USE mod_global

  IMPLICIT NONE
CONTAINS

  FUNCTION kiss64()
    INTEGER(i8b), SAVE :: x, y, z, c
    INTEGER(i8b) :: t, k, m, s, kiss64
    DATA x, y, z, c &
         / 1234567890987654321_i8b, &
         362436362436362436_i8b, &
         1066149217761810_i8b, &
         123456123456123456_i8b /
    m(x,k) = IEOR(x, ISHFT(x,k))  ! statement function
    s(x) = ISHFT(x, -63)          ! statement function
    t = ISHFT(x, 58) + c
    IF (s(x) .EQ. s(t)) THEN
       c = ISHFT(x, -6) + s(x)
    ELSE
       c = ISHFT(x, -6) + 1 - s(x + t)
    ENDIF
    x = t + x
    y = m(m(m(y,13_i8b),-17_i8b), 43_i8b)
    z = 6906969069_i8b * z + 1234567
    kiss64 = x + y + z
  END FUNCTION kiss64
END MODULE mod_rand
